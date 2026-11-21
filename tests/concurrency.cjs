'use strict';

// End-to-end concurrency contract for the local HTTP server.
//
// What it proves: a slow request must not delay a fast one. The server used to
// accept one connection at a time, so a three-second export blocked every
// other request behind it, and the page appeared to hang.
//
// Run it against a server that is already listening, or let the wrapper start
// one. Usage:
//   node tests/concurrency.cjs --port 18987 --expect-workers 1
//   node tests/concurrency.cjs --port 18987 --slow-ms 3000 --poller-idle-ms 41000
//
// The slow endpoint only exists when PA_SERVE_TEST_SLOW_MS is set in the
// server environment, so production traffic can never reach it.

const http = require('node:http');

function arg(name, fallback) {
  const i = process.argv.indexOf(`--${name}`);
  if (i < 0 || i + 1 >= process.argv.length) return fallback;
  return process.argv[i + 1];
}

const PORT = Number(arg('port', 18987));
const HOST = '127.0.0.1';
const SLOW_MS = Number(arg('slow-ms', 3000));
// A fast request that shares the machine with a slow one must still come back
// in roughly a single round trip. The Windows select timer floor is ~200 ms;
// the accept/worker path must not pay it.
const FAST_BUDGET_MS = Number(arg('fast-budget-ms', 200));
const STATIC_BUDGET_MS = Number(arg('static-budget-ms', 1000));
const STATIC_P95_BUDGET_MS = Number(arg('static-p95-budget-ms', 600));
const EXPECT_WORKERS = Number(arg('expect-workers', 0));
const POLLER_IDLE_MS = Number(arg('poller-idle-ms', 0));

let pass = 0;
let fail = 0;
function check(name, ok, detail = '') {
  if (ok) {
    pass += 1;
    console.log(`  [ok]   ${name}`);
  } else {
    fail += 1;
    console.log(`  [FAIL] ${name}  ${detail}`);
  }
}

function req(path) {
  return new Promise((resolve, reject) => {
    const t0 = process.hrtime.bigint();
    const r = http.request({ host: HOST, port: PORT, path, method: 'GET' }, (res) => {
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => {
        const t1 = process.hrtime.bigint();
        const body = Buffer.concat(chunks);
        resolve({
          status: res.statusCode,
          bytes: body.length,
          body: body.toString('utf8'),
          ms: Number(t1 - t0) / 1e6,
        });
      });
    });
    r.on('error', reject);
    r.setTimeout(120000, () => r.destroy(new Error('timeout')));
    r.end();
  });
}

function percentile(values, fraction) {
  const sorted = [...values].sort((a, b) => a - b);
  const i = Math.min(sorted.length - 1, Math.floor(sorted.length * fraction));
  return sorted[i];
}

async function main() {
  const baseline = await req('/app.js');
  check('server answers a static asset', baseline.status === 200 && baseline.bytes > 0,
    `status=${baseline.status} bytes=${baseline.bytes}`);

  // Warm the worker pool and the JIT so the first-request cost does not show up
  // as a concurrency failure.
  await req('/app.js');
  const pingWarm = await req('/api/ping');
  const pingMeta = JSON.parse(pingWarm.body);
  if (EXPECT_WORKERS > 0) {
    check('server reports the expected worker target',
      Number(pingMeta.workers_target) === EXPECT_WORKERS,
      `workers_target=${pingMeta.workers_target} expected=${EXPECT_WORKERS}`);
  }

  // A. Ten page assets in parallel must not take ten select floors.
  const tenStart = process.hrtime.bigint();
  const ten = await Promise.all(Array.from({ length: 10 }, () => req('/app.js')));
  const tenWall = Number(process.hrtime.bigint() - tenStart) / 1e6;
  const tenP95 = percentile(ten.map((x) => x.ms), 0.95);
  check('ten parallel static requests all succeed',
    ten.every((x) => x.status === 200 && x.bytes > 0));
  check('ten parallel static requests finish within budget',
    tenWall < STATIC_BUDGET_MS,
    `wall=${tenWall.toFixed(0)} ms budget=${STATIC_BUDGET_MS} ms`);
  check('ten parallel static P95 within budget',
    tenP95 < STATIC_P95_BUDGET_MS,
    `p95=${tenP95.toFixed(0)} ms budget=${STATIC_P95_BUDGET_MS} ms`);

  // B. One slow worker must not own the master's accept path.
  const slowPromises = Array.from({ length: 3 }, () => req(`/__test_slow?ms=${SLOW_MS}`));
  // Give the slow request a head start so it really is in flight.
  await new Promise((r) => setTimeout(r, 150));

  const fastSlowWindow = await req('/api/ping');
  check('slow request in flight does not delay /api/ping',
    fastSlowWindow.status === 200 && fastSlowWindow.ms < FAST_BUDGET_MS,
    `ping took ${fastSlowWindow.ms.toFixed(0)} ms while ${SLOW_MS} ms requests were running`);

  const staticDuringSlow = await req('/app.js');
  check('slow request in flight does not delay a static asset',
    staticDuringSlow.status === 200 && staticDuringSlow.ms < FAST_BUDGET_MS,
    `app.js took ${staticDuringSlow.ms.toFixed(0)} ms`);

  const slowResults = await Promise.all(slowPromises);
  check('slow requests eventually complete',
    slowResults.every((x) => x.status === 200 && x.ms >= SLOW_MS * 0.9),
    slowResults.map((x) => `${x.status}/${x.ms.toFixed(0)}ms`).join(','));

  // C. Six parallel static requests must not be six times one request.
  // One worker is deliberately the serialised case used by requirement B;
  // run this check against a pool with at least two workers.
  const one = (await req('/app.js')).ms;
  let wall = 0;
  if (EXPECT_WORKERS === 1) {
    console.log('  [skip] six-way static parallelism needs at least two workers');
  } else {
    const t0 = process.hrtime.bigint();
    const many = await Promise.all(Array.from({ length: 6 }, () => req('/app.js')));
    wall = Number(process.hrtime.bigint() - t0) / 1e6;
    const serialEstimate = one * 6;
    check('six parallel requests are not serialised',
      many.every((x) => x.status === 200) && wall < serialEstimate * 0.7,
      `wall=${wall.toFixed(0)} ms vs serial estimate ${serialEstimate.toFixed(0)} ms`);
  }

  // D. The poller is process state, not a side effect of a page request.
  // Let the queue sit completely idle before reading the heartbeat.
  if (POLLER_IDLE_MS > 0) {
    await new Promise((r) => setTimeout(r, POLLER_IDLE_MS));
  }
  const status = await req('/api/poller/status');
  let statusBody = null;
  try { statusBody = JSON.parse(status.body); } catch {}
  check('poller status responds', status.status === 200 && statusBody && statusBody.ok === true,
    `status=${status.status}`);
  check('poller heartbeat is present after idle',
    Boolean(statusBody?.poller?.last_run_at),
    `last_run_at=${JSON.stringify(statusBody?.poller?.last_run_at)}`);

  console.log(`\nPASS=${pass} FAIL=${fail}`);
  console.log(`baseline /app.js=${one.toFixed(0)} ms; 10x wall=${tenWall.toFixed(0)} ms; 6x wall=${wall.toFixed(0)} ms`);
  process.exit(fail === 0 ? 0 : 1);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
