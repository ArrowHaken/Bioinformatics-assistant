(() => {
  "use strict";

  if (window.__catscoArtifactContextBridgeV1) return;
  window.__catscoArtifactContextBridgeV1 = true;

  // 受信宿主父域。宿主若不是这些域，连接消息会被丢弃。
  //   2026-10-25 扩：平台侧的会话界面并不固定在 app.catsco.cc。
  //   实测日志里出现过 app.catsco.cc / app.catsco.cn / agent-*.artifacts.catsco.fun，
  //   按旧白名单只认前两个，导致"宿主明明在跟我们说话，消息却被丢掉"，
  //   表现就是任务通道永远 host_not_connected。
  //   现在改成"域名后缀属于本平台的几个域"就接受，仍然挡住任意外部站点。
  const TRUSTED_PARENT_HOSTS = new Set(["app.catsco.cc","app.catsco.cn"]);
  const TRUSTED_PARENT_SUFFIXES = [".catsco.cc", ".catsco.cn", ".catsco.fun"];

  const REQUEST_TYPE = "catsco.artifact.context.request.v1";
  const RESPONSE_TYPE = "catsco.artifact.context.response.v1";
  const CONTEXT_CONTRACT = "catsco.artifact-page-context.v1";
  const RESULT_REQUEST_TYPE = "catsco.artifact.result.request.v1";
  const RESULT_RESPONSE_TYPE = "catsco.artifact.result.response.v1";
  const RESULT_CONTRACT = "catsco.artifact-result.v1";
  const RESULT_RECEIPT_CONTRACT = "catsco.artifact-result-receipt.v1";
  const BRIDGE_READY_TYPE = "catsco.artifact.bridge.ready.v1";
  const HOST_CONNECT_TYPE = "catsco.artifact.host.connect.v1";
  const TASK_REQUEST_TYPE = "catsco.artifact.task.request.v1";
  const TASK_ACCEPTED_TYPE = "catsco.artifact.task.accepted.v1";
  const TASK_REJECTED_TYPE = "catsco.artifact.task.rejected.v1";
  const TASK_STATUS_TYPE = "catsco.artifact.task.status.v1";
  const TASK_STATUS_CONTRACT = "catsco.artifact-task-status.v1";
  const RUNTIME_REQUEST_TYPE = "catsco.artifact.runtime.request.v1";
  const RUNTIME_RESPONSE_TYPE = "catsco.artifact.runtime.response.v1";
  const RUNTIME_EVENT_TYPE = "catsco.artifact.runtime.event.v1";
  const RUNTIME_RESPONSE_CONTRACT = "catsco.artifact-runtime-response.v1";
  const RUNTIME_STATE_CONTRACT = "catsco.artifact-runtime-state.v1";
  const RUNTIME_EVENT_CONTRACT_V1 = "catsco.artifact-runtime-event.v1";
  const RUNTIME_EVENT_CONTRACT_V2 = "catsco.artifact-runtime-event.v2";
  const RUNTIME_RUN_CONTRACT = "catsco.artifact-runtime-run.v1";
  const RUNTIME_VERSIONS = new Set(["0.1", "0.2"]);
  const MAX_RESULT_BYTES = 64 * 1024;
  const MAX_RECEIPT_BYTES = 8 * 1024;
  const MAX_RESULT_DEPTH = 12;
  const MAX_RESULT_VISITS = 16_384;
  const MAX_RESULT_ARRAY_ITEMS = 1_000;
  const MAX_RESULT_OBJECT_KEYS = 256;
  const MAX_RESULT_KEY_LENGTH = 128;
  const MAX_RESULT_STRING_LENGTH = 16_384;
  const MAX_RECEIPT_STRING_LENGTH = 2_000;
  const MAX_RESULT_CACHE = 128;
  const MAX_TASK_CACHE = 128;
  const TASK_REQUEST_TIMEOUT_MS = 15_000;
  const RUNTIME_REQUEST_TIMEOUT_MS = 15_000;
  const RUNTIME_HOST_WAIT_TIMEOUT_MS = 10_000;
  const RUNTIME_HOST_READY_RETRY_MS = 500;
  const MAX_RUNTIME_VIEW_BYTES = 4 * 1024;
  const MAX_RUNTIME_STATE_BYTES = 256 * 1024;
  const MAX_RUNTIME_MESSAGE_BYTES = 300 * 1024;
  const MAX_RUNTIME_STATE_VISITS = 32_768;
  const TASK_USER_ACTIVATION_MAX_AGE_MS = 2_000;
  const MAX_CONTROLS = 24;
  const MAX_SEMANTIC_BYTES = 8 * 1024;
  const MAX_SEMANTIC_DEPTH = 6;
  const MAX_SEMANTIC_ARRAY_ITEMS = 50;
  const MAX_SEMANTIC_OBJECT_KEYS = 50;
  const MAX_SEMANTIC_KEY_LENGTH = 128;
  const MAX_SEMANTIC_STRING_LENGTH = 1000;
  const MAX_SEMANTIC_VISITS = 4096;
  const INVALID_SEMANTIC_VALUE = Symbol("invalid-semantic-value");
  const UNSAFE_SEMANTIC_KEYS = new Set(["__proto__", "constructor", "prototype"]);
  const CONTROL_TYPES = new Set([
    "checkbox", "radio", "select-one", "select-multiple",
    "text", "search", "number", "range", "textarea"
  ]);

  let lastInteraction = null;
  let lastControl = null;
  let lastTrustedTaskActivationAt = 0;
  let taskActivationSerial = 0;
  let consumedTaskActivationSerial = 0;
  let connectedParentOrigin = "";
  const resultDeliveries = new Map();
  const pendingTaskRequests = new Map();
  const taskStatuses = new Map();
  const taskStatusListeners = new Set();
  const pendingRuntimeRequests = new Map();
  const runtimeEventListeners = new Set();
  const runtimeHostWaiters = new Set();
  let runtimeConnection = null;
  let runtimeView = null;
  let runtimeSubscriptionPending = null;

  document.addEventListener("click", rememberInteraction, true);
  document.addEventListener("change", rememberInteraction, true);
  document.addEventListener("input", rememberInteraction, true);
  window.addEventListener("message", handleMessage);
  installTaskAPI();
  installRuntimeAPI();
  announceReady();

  function handleMessage(event) {
    if (event.source !== window.parent || !allowedParentOrigin(event.origin)) return;
    const data = event.data;
    if (!plainObject(data)) return;
    if (data.type === HOST_CONNECT_TYPE) {
      connectedParentOrigin = event.origin;
      for (const resolve of runtimeHostWaiters) {
        try { resolve(true); } catch {}
      }
      runtimeHostWaiters.clear();
      return;
    }
    if (connectedParentOrigin && event.origin !== connectedParentOrigin) return;
    connectedParentOrigin = event.origin;
    if (data.type === TASK_STATUS_TYPE) {
      handleTaskStatus(data);
      return;
    }
    if (data.type === RUNTIME_EVENT_TYPE) {
      handleRuntimeEvent(data);
      return;
    }
    if (!validRequestId(data.request_id)) return;
    if (data.type === RUNTIME_RESPONSE_TYPE) {
      handleRuntimeResponse(data);
      return;
    }
    if (data.type === REQUEST_TYPE) handleContextRequest(event, data);
    if (data.type === RESULT_REQUEST_TYPE) void handleResultRequest(event, data);
    if (data.type === TASK_ACCEPTED_TYPE) handleTaskAccepted(data);
    if (data.type === TASK_REJECTED_TYPE) handleTaskRejected(data);
  }

  function handleContextRequest(event, data) {
    event.source.postMessage({
      type: RESPONSE_TYPE,
      request_id: data.request_id,
      context: collectContext()
    }, event.origin);
  }

  function installTaskAPI() {
    const api = Object.freeze({
      requestTask,
      onTaskStatus(listener) {
        if (typeof listener !== "function") return () => {};
        taskStatusListeners.add(listener);
        return () => taskStatusListeners.delete(listener);
      },
      getTaskStatus(taskId) {
        const status = taskStatuses.get(taskId);
        return status ? { ...status } : null;
      },
      isConnected() {
        return Boolean(connectedParentOrigin);
      }
    });
    try {
      Object.defineProperty(window, "catscoArtifactHost", {
        value: api,
        enumerable: true,
        configurable: false,
        writable: false
      });
    } catch {
      try { window.catscoArtifactHost = api; } catch {}
    }
  }

  function installRuntimeAPI() {
    const api = Object.freeze({
      connect: connectRuntime,
      state: Object.freeze({
        get: runtimeStateGet,
        list: runtimeStateList,
        put: runtimeStatePut,
        patch: runtimeStatePatch
      }),
      runs: Object.freeze({
        get: runtimeRunGet,
        list: runtimeRunList
      }),
      view: Object.freeze({
        update: runtimeViewUpdate,
        get() {
          return runtimeView ? cloneRuntimeValue(runtimeView) : null;
        }
      }),
      subscribe: subscribeRuntime,
      isConnected() {
        return Boolean(connectedParentOrigin && runtimeConnection);
      }
    });
    try {
      Object.defineProperty(window, "catscoArtifactRuntime", {
        value: api,
        enumerable: true,
        configurable: false,
        writable: false
      });
    } catch {
      try { window.catscoArtifactRuntime = api; } catch {}
    }
  }

  async function connectRuntime() {
    const response = await runtimeRequest("connect", {});
    if (!plainObject(response) || response.ok !== true
        || response.contract_version !== RUNTIME_RESPONSE_CONTRACT
        || response.operation !== "connect" || !plainObject(response.runtime)
        || !RUNTIME_VERSIONS.has(response.runtime.version)
        || !Array.isArray(response.runtime.surfaces)
        || !Array.isArray(response.runtime.state)
        || !Number.isSafeInteger(response.event_cursor) || response.event_cursor < 0) {
      throw runtimeError("invalid_response", "Artifact Runtime connection response is invalid");
    }
    const runs = response.runtime.version === "0.2"
      ? normalizedRuntimeRuns(response.runs)
      : [];
    const normalized = cloneRuntimeValue({
      artifact: response.artifact,
      runtime: response.runtime,
      event_cursor: response.event_cursor,
      runs
    });
    if (normalized === INVALID_SEMANTIC_VALUE || !plainObject(normalized)) {
      throw runtimeError("invalid_response", "Artifact Runtime connection response is invalid");
    }
    runtimeConnection = normalized;
    return cloneRuntimeValue(normalized);
  }

  async function runtimeStateGet(namespace, key) {
    await ensureRuntimeConnected();
    const target = runtimeStateTarget(namespace, key);
    const response = await runtimeRequest("state.get", target);
    return normalizedRuntimeState(response && response.state);
  }

  async function runtimeStateList() {
    await ensureRuntimeConnected();
    const response = await runtimeRequest("state.list", {});
    if (!plainObject(response) || response.ok !== true || !Array.isArray(response.state_refs)) {
      throw runtimeError("invalid_response", "Artifact Runtime State list is invalid");
    }
    const refs = cloneRuntimeValue(response.state_refs);
    if (refs === INVALID_SEMANTIC_VALUE || !Array.isArray(refs)) {
      throw runtimeError("invalid_response", "Artifact Runtime State list is invalid");
    }
    return refs;
  }

  async function runtimeRunGet(runId) {
    await ensureRuntimeConnected();
    requireRuntime02();
    const normalizedRunId = runtimeExact(runId, 47);
    if (!/^run_[A-Za-z0-9_-]{43}$/.test(normalizedRunId)) {
      throw runtimeError("invalid_run", "Artifact Runtime Run ID is invalid");
    }
    const response = await runtimeRequest("run.get", { run_id: normalizedRunId });
    return normalizedRuntimeRun(response && response.run);
  }

  async function runtimeRunList() {
    await ensureRuntimeConnected();
    requireRuntime02();
    const response = await runtimeRequest("run.list", {});
    return normalizedRuntimeRuns(response && response.runs);
  }

  async function runtimeStatePut(namespace, key, value, baseRevision) {
    await ensureRuntimeConnected();
    const target = runtimeStateTarget(namespace, key);
    if (!runtimeRevision(baseRevision)) {
      throw runtimeError("invalid_revision", "Artifact Runtime base revision is invalid");
    }
    const normalizedValue = cloneRuntimeValue(
      value,
      MAX_RUNTIME_STATE_BYTES,
      MAX_RUNTIME_STATE_VISITS
    );
    if (normalizedValue === INVALID_SEMANTIC_VALUE) {
      throw runtimeError("invalid_value", "Artifact Runtime State value is invalid");
    }
    const response = await runtimeRequest("state.put", {
      ...target,
      base_revision: baseRevision,
      value: normalizedValue
    });
    return normalizedRuntimeApplied(response);
  }

  async function runtimeStatePatch(namespace, key, operations, baseRevision) {
    await ensureRuntimeConnected();
    const target = runtimeStateTarget(namespace, key);
    if (!runtimeRevision(baseRevision) || baseRevision <= 0
        || !Array.isArray(operations) || operations.length === 0 || operations.length > 64) {
      throw runtimeError("invalid_patch", "Artifact Runtime State patch is invalid");
    }
    const patch = cloneRuntimeValue(
      operations,
      MAX_RUNTIME_STATE_BYTES,
      MAX_RUNTIME_STATE_VISITS
    );
    if (!Array.isArray(patch)) {
      throw runtimeError("invalid_patch", "Artifact Runtime State patch is invalid");
    }
    const response = await runtimeRequest("state.patch", {
      ...target,
      base_revision: baseRevision,
      patch
    });
    return normalizedRuntimeApplied(response);
  }

  async function runtimeViewUpdate(value) {
    await ensureRuntimeConnected();
    const normalized = cloneRuntimeValue(value, MAX_RUNTIME_VIEW_BYTES);
    if (!plainObject(normalized)) {
      throw runtimeError("invalid_view", "Artifact Runtime View must be an object");
    }
    const surface = runtimeExact(normalized.surface, 64);
    const declared = runtimeConnection.runtime.surfaces.some(entry => (
      plainObject(entry) && entry.id === surface
    ));
    if (!surface || !declared) {
      throw runtimeError("surface_not_declared", "Artifact Runtime View surface is not declared");
    }
    normalized.surface = surface;
    runtimeView = normalized;
    return cloneRuntimeValue(runtimeView);
  }

  function subscribeRuntime(listener) {
    if (typeof listener !== "function") return () => {};
    runtimeEventListeners.add(listener);
    if (runtimeEventListeners.size === 1) {
      runtimeSubscriptionPending = ensureRuntimeConnected()
        .then(() => runtimeRequest("events.subscribe", {
          after_event_id: runtimeConnection.event_cursor
        }))
        .then(response => {
          if (Number.isSafeInteger(response && response.event_cursor)
              && response.event_cursor >= runtimeConnection.event_cursor) {
            runtimeConnection.event_cursor = response.event_cursor;
          }
        })
        .catch(error => {
          for (const current of runtimeEventListeners) {
            try { current({ type: "runtime.error", error: runtimePublicError(error) }); } catch {}
          }
        })
        .finally(() => { runtimeSubscriptionPending = null; });
    }
    return () => {
      runtimeEventListeners.delete(listener);
      if (runtimeEventListeners.size === 0) {
        void Promise.resolve(runtimeSubscriptionPending).finally(() => {
          if (runtimeEventListeners.size === 0 && connectedParentOrigin) {
            void runtimeRequest("events.unsubscribe", {}).catch(() => {});
          }
        });
      }
    };
  }

  function handleRuntimeResponse(data) {
    const pending = pendingRuntimeRequests.get(data.request_id);
    if (!pending) return;
    window.clearTimeout(pending.timer);
    pendingRuntimeRequests.delete(data.request_id);
    const response = cloneRuntimeValue(
      data.response,
      MAX_RUNTIME_MESSAGE_BYTES,
      MAX_RUNTIME_STATE_VISITS
    );
    if (!plainObject(response)) {
      pending.reject(runtimeError("invalid_response", "Artifact Runtime response is invalid"));
      return;
    }
    if (response.ok !== true) {
      const source = plainObject(response.error) ? response.error : {};
      const error = runtimeError(
        runtimeExact(source.code, 128) || "runtime_request_failed",
        runtimeExact(source.message, 500) || "Artifact Runtime request failed"
      );
      if (Number.isSafeInteger(source.current_revision) && source.current_revision >= 0) {
        error.currentRevision = source.current_revision;
      }
      pending.reject(error);
      return;
    }
    pending.resolve(response);
  }

  function handleRuntimeEvent(data) {
    const event = normalizedRuntimeEvent(data.event);
    if (!event) return;
    if (runtimeConnection && event.event_id > runtimeConnection.event_cursor) {
      runtimeConnection.event_cursor = event.event_id;
    }
    for (const listener of runtimeEventListeners) {
      try { listener(event); } catch {}
    }
  }

  function runtimeRequest(operation, payload) {
    if (!connectedParentOrigin || !allowedParentOrigin(connectedParentOrigin)) {
      return waitForRuntimeHost().then(connected => {
        if (!connected) {
          throw runtimeError("host_not_connected", "CatsCo Host is not connected");
        }
        return runtimeRequest(operation, payload);
      });
    }
    const requestId = taskRequestId();
    return new Promise((resolve, reject) => {
      const timer = window.setTimeout(() => {
        pendingRuntimeRequests.delete(requestId);
        reject(runtimeError("request_timeout", "Artifact Runtime request timed out"));
      }, RUNTIME_REQUEST_TIMEOUT_MS);
      pendingRuntimeRequests.set(requestId, { resolve, reject, timer });
      try {
        window.parent.postMessage({
          type: RUNTIME_REQUEST_TYPE,
          request_id: requestId,
          operation,
          payload
        }, connectedParentOrigin);
      } catch {
        window.clearTimeout(timer);
        pendingRuntimeRequests.delete(requestId);
        reject(runtimeError("request_failed", "Artifact Runtime request could not be sent"));
      }
    });
  }

  function waitForRuntimeHost() {
    if (connectedParentOrigin && allowedParentOrigin(connectedParentOrigin)) {
      return Promise.resolve(true);
    }
    if (!window.parent || window.parent === window) return Promise.resolve(false);
    return new Promise(resolve => {
      let settled = false;
      let retryTimer = 0;
      let timeoutTimer = 0;
      const finish = value => {
        if (settled) return;
        settled = true;
        if (retryTimer) window.clearTimeout(retryTimer);
        if (timeoutTimer) window.clearTimeout(timeoutTimer);
        runtimeHostWaiters.delete(finish);
        resolve(value);
      };
      const retryReady = () => {
        if (settled) return;
        announceReady();
        if (!settled) {
          retryTimer = window.setTimeout(retryReady, RUNTIME_HOST_READY_RETRY_MS);
        }
      };
      runtimeHostWaiters.add(finish);
      retryReady();
      if (!settled) {
        timeoutTimer = window.setTimeout(() => finish(false), RUNTIME_HOST_WAIT_TIMEOUT_MS);
      }
    });
  }

  function ensureRuntimeConnected() {
    return runtimeConnection ? Promise.resolve(runtimeConnection) : connectRuntime();
  }

  function runtimeStateTarget(namespace, key) {
    const normalizedNamespace = runtimeExact(namespace, 64);
    const normalizedKey = runtimeExact(key, 128);
    if (!/^[a-z][a-z0-9]*(?:[._-][a-z0-9]+)*$/.test(normalizedNamespace)
        || !/^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$/.test(normalizedKey)) {
      throw runtimeError("invalid_state_key", "Artifact Runtime State key is invalid");
    }
    return { namespace: normalizedNamespace, key: normalizedKey };
  }

  function normalizedRuntimeState(value) {
    const state = cloneRuntimeValue(
      value,
      MAX_RUNTIME_MESSAGE_BYTES,
      MAX_RUNTIME_STATE_VISITS
    );
    if (!plainObject(state) || state.contract_version !== RUNTIME_STATE_CONTRACT
        || typeof state.exists !== "boolean" || !runtimeRevision(state.revision)) {
      throw runtimeError("invalid_response", "Artifact Runtime State response is invalid");
    }
    return state;
  }

  function normalizedRuntimeApplied(response) {
    if (!plainObject(response) || response.ok !== true || response.applied !== true) {
      throw runtimeError("not_applied", "Artifact Runtime State was not applied");
    }
    return normalizedRuntimeState(response.state);
  }

  function normalizedRuntimeRuns(value) {
    if (!Array.isArray(value) || value.length > 20) {
      throw runtimeError("invalid_response", "Artifact Runtime Run list is invalid");
    }
    return value.map(normalizedRuntimeRun);
  }

  function normalizedRuntimeRun(value) {
    const run = cloneRuntimeValue(value, 32 * 1024);
    if (!plainObject(run) || run.contract_version !== RUNTIME_RUN_CONTRACT
        || !/^atk_[A-Za-z0-9_-]{43}$/.test(String(run.task_id || ""))
        || !/^run_[A-Za-z0-9_-]{43}$/.test(String(run.run_id || ""))
        || !runtimeExact(run.action_id, 128)
        || !new Set(["submitted", "running", "completed", "failed"]).has(run.status)
        || !runtimeExact(run.artifact_id, 64)
        || !Number.isSafeInteger(run.displayed_version) || run.displayed_version <= 0
        || !runtimeExact(run.topic_id, 512) || !runtimeExact(run.created_at, 64)
        || !runtimeExact(run.updated_at, 64) || !runtimeExact(run.expires_at, 64)) {
      throw runtimeError("invalid_response", "Artifact Runtime Run response is invalid");
    }
    return run;
  }

  function normalizedRuntimeEvent(value) {
    const event = cloneRuntimeValue(value, 32 * 1024);
    if (!plainObject(event) || !Number.isSafeInteger(event.event_id) || event.event_id <= 0) {
      return null;
    }
    if (event.contract_version === RUNTIME_EVENT_CONTRACT_V1) {
      return event.type === "state.updated" && Number.isSafeInteger(event.revision)
        && event.revision > 0 ? event : null;
    }
    if (event.contract_version !== RUNTIME_EVENT_CONTRACT_V2
        || !new Set(["state.updated", "run.started", "result.applied", "run.finished", "run.error"]).has(event.type)
        || !runtimeExact(event.artifact_id, 64) || !runtimeExact(event.created_at, 64)
        || !plainObject(event.data)) return null;
    if (event.run_id !== undefined && !/^run_[A-Za-z0-9_-]{43}$/.test(String(event.run_id))) return null;
    if (event.task_id !== undefined && !/^atk_[A-Za-z0-9_-]{43}$/.test(String(event.task_id))) return null;
    if (event.type !== "state.updated" && (!event.run_id || !event.task_id)) return null;
    if (event.type === "state.updated"
        && (!runtimeExact(event.data.namespace, 64) || !runtimeExact(event.data.key, 128)
          || !Number.isSafeInteger(event.data.revision) || event.data.revision <= 0)) return null;
    if (event.type === "state.updated") {
      event.namespace = event.data.namespace;
      event.key = event.data.key;
      event.revision = event.data.revision;
    }
    return event;
  }

  function requireRuntime02() {
    if (runtimeConnection.runtime.version !== "0.2") {
      throw runtimeError("runs_not_enabled", "Artifact Runtime Runs are not enabled");
    }
  }

  function runtimeRevision(value) {
    return Number.isSafeInteger(value) && value >= 0;
  }

  function runtimeError(code, message) {
    const error = new Error(message);
    error.code = code;
    return error;
  }

  function runtimePublicError(error) {
    return {
      code: runtimeExact(error && error.code, 128) || "runtime_request_failed",
      message: runtimeExact(error && error.message, 500) || "Artifact Runtime request failed",
      ...(Number.isSafeInteger(error && error.currentRevision)
        ? { current_revision: error.currentRevision }
        : {})
    };
  }

  function cloneRuntimeValue(
    value,
    maximumBytes = MAX_RESULT_BYTES,
    maximumVisits = MAX_RESULT_VISITS
  ) {
    const cloned = cloneBoundedResultJson(value, {
      maxBytes: maximumBytes,
      maxStringLength: MAX_RESULT_STRING_LENGTH,
      maxVisits: maximumVisits
    });
    return cloned;
  }

  function runtimeExact(value, limit) {
    return typeof value === "string" && value.length > 0 && value.length <= limit
      && value === value.trim() && !/[\0\r\n]/.test(value)
      ? value
      : "";
  }

  function announceReady() {
    try {
      if (window.parent && window.parent !== window) {
        window.parent.postMessage({ type: BRIDGE_READY_TYPE }, "*");
      }
    } catch {}
  }

  function requestTask(intentId, payload) {
    const normalizedIntentId = cleanExact(intentId, 128);
    if (!validResultSinkId(normalizedIntentId)) {
      return Promise.reject(taskError("invalid_intent", "Artifact task intent is invalid"));
    }
    if (!connectedParentOrigin || !allowedParentOrigin(connectedParentOrigin)) {
      announceReady();
      return Promise.reject(taskError("host_not_connected", "CatsCo Host is not connected"));
    }
    const normalizedPayload = cloneBoundedResultJson(payload, {
      maxBytes: MAX_RESULT_BYTES,
      maxStringLength: MAX_RESULT_STRING_LENGTH,
      maxVisits: MAX_RESULT_VISITS
    });
    if (normalizedPayload === INVALID_SEMANTIC_VALUE) {
      return Promise.reject(taskError("invalid_payload", "Artifact task payload is invalid"));
    }
    if (!consumeTaskUserActivation()) {
      return Promise.reject(taskError(
        "user_activation_required",
        "Artifact tasks must be submitted from an explicit user action"
      ));
    }
    const requestId = taskRequestId();
    return new Promise((resolve, reject) => {
      const timer = window.setTimeout(() => {
        pendingTaskRequests.delete(requestId);
        reject(taskError("request_timeout", "Artifact task request timed out"));
      }, TASK_REQUEST_TIMEOUT_MS);
      pendingTaskRequests.set(requestId, { resolve, reject, timer });
      try {
        window.parent.postMessage({
          type: TASK_REQUEST_TYPE,
          request_id: requestId,
          intent_id: normalizedIntentId,
          payload: normalizedPayload
        }, connectedParentOrigin);
      } catch {
        window.clearTimeout(timer);
        pendingTaskRequests.delete(requestId);
        reject(taskError("request_failed", "Artifact task request could not be sent"));
      }
    });
  }

  function handleTaskAccepted(data) {
    const pending = pendingTaskRequests.get(data.request_id);
    if (!pending) return;
    const status = normalizeTaskStatus(data.task);
    if (!status || status.status !== "submitted") {
      handleTaskRejected({
        request_id: data.request_id,
        code: "invalid_response",
        message: "Artifact task response is invalid"
      });
      return;
    }
    window.clearTimeout(pending.timer);
    pendingTaskRequests.delete(data.request_id);
    rememberTaskStatus(status);
    pending.resolve({ ...status });
  }

  function handleTaskRejected(data) {
    const pending = pendingTaskRequests.get(data.request_id);
    if (!pending) return;
    window.clearTimeout(pending.timer);
    pendingTaskRequests.delete(data.request_id);
    const code = cleanExact(data.code, 64) || "request_rejected";
    const message = cleanExact(data.message, 500) || "Artifact task request was rejected";
    pending.reject(taskError(code, message));
  }

  function handleTaskStatus(data) {
    const status = normalizeTaskStatus(data.task);
    if (status) rememberTaskStatus(status);
  }

  function rememberTaskStatus(status) {
    taskStatuses.set(status.task_id, status);
    while (taskStatuses.size > MAX_TASK_CACHE) {
      taskStatuses.delete(taskStatuses.keys().next().value);
    }
    for (const listener of taskStatusListeners) {
      try { listener({ ...status }); } catch {}
    }
  }

  function normalizeTaskStatus(value) {
    if (!plainObject(value) || value.contract_version !== TASK_STATUS_CONTRACT
        || !validTaskId(value.task_id)
        || !new Set(["submitted", "running", "completed", "failed"]).has(value.status)) return null;
    const status = {
      contract_version: TASK_STATUS_CONTRACT,
      task_id: value.task_id,
      status: value.status
    };
    for (const [key, limit] of [["code", 64], ["message", 500], ["run_id", 128], ["executor_run_id", 128], ["result_id", 64], ["completion_mode", 32], ["updated_at", 64], ["expires_at", 64]]) {
      if (value[key] === undefined || value[key] === "") continue;
      const text = cleanExact(value[key], limit);
      if (!text || text !== value[key] || /[\0\r\n]/.test(text)) return null;
      status[key] = text;
    }
    if (status.code && !/^[a-z][a-z0-9_]{0,63}$/.test(status.code)) return null;
    if (status.result_id && !validResultId(status.result_id)) return null;
    return status;
  }

  function taskRequestId() {
    try {
      if (window.crypto && typeof window.crypto.randomUUID === "function") {
        return window.crypto.randomUUID();
      }
    } catch {}
    return `artifact-task-${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`;
  }

  function taskError(code, message) {
    const error = new Error(message);
    error.code = code;
    return error;
  }

  async function handleResultRequest(event, data) {
    const result = normalizeResultRequest(data.result);
    if (!result) return;
    const receipt = await applyResultOnce(result);
    event.source.postMessage({
      type: RESULT_RESPONSE_TYPE,
      request_id: data.request_id,
      receipt
    }, event.origin);
  }

  function applyResultOnce(result) {
    if (resultDeliveries.has(result.result_id)) {
      return resultDeliveries.get(result.result_id);
    }
    const delivery = applyResult(result).catch(() => resultReceipt(
      result.result_id,
      "failed",
      "application_error"
    ));
    resultDeliveries.set(result.result_id, delivery);
    while (resultDeliveries.size > MAX_RESULT_CACHE) {
      resultDeliveries.delete(resultDeliveries.keys().next().value);
    }
    return delivery;
  }

  async function applyResult(result) {
    if (!resultMatchesCurrentPage(result)) {
      return resultReceipt(result.result_id, "rejected", "target_mismatch");
    }
    let api;
    let handler;
    try {
      api = window.catscoArtifact;
      handler = api && api.applyResult;
    } catch {
      return resultReceipt(result.result_id, "failed", "result_sink_unavailable");
    }
    if (typeof handler !== "function") {
      return resultReceipt(result.result_id, "rejected", "result_sink_unavailable");
    }

    let response;
    try {
      response = await Promise.resolve(handler.call(api, cloneResultForApplication(result)));
    } catch {
      return resultReceipt(
        result.result_id,
        "failed",
        "application_error"
      );
    }
    return normalizeApplicationReceipt(response, result.result_id);
  }

  function normalizeResultRequest(value) {
    if (!plainObject(value)) return null;
    const allowed = new Set([
      "contract_version",
      "artifact_id",
      "displayed_version",
      "sink_id",
      "result_id",
      "expected_state_revision",
      "payload"
    ]);
    if (!safeObjectKeys(value, allowed)) return null;
    if (value.contract_version !== RESULT_CONTRACT) return null;
    if (!validArtifactId(value.artifact_id) || !validResultSinkId(value.sink_id)
        || !validResultId(value.result_id)) return null;
    if (!Number.isSafeInteger(value.displayed_version) || value.displayed_version <= 0) return null;
    const revision = optionalResultRevision(value.expected_state_revision);
    if (revision === false) return null;
    const payload = cloneBoundedResultJson(value.payload, {
      maxBytes: MAX_RESULT_BYTES,
      maxStringLength: MAX_RESULT_STRING_LENGTH,
      maxVisits: MAX_RESULT_VISITS
    });
    if (payload === INVALID_SEMANTIC_VALUE) return null;
    const result = {
      contract_version: RESULT_CONTRACT,
      artifact_id: value.artifact_id,
      displayed_version: value.displayed_version,
      sink_id: value.sink_id,
      result_id: value.result_id,
      payload
    };
    if (revision !== null) result.expected_state_revision = revision;
    return result;
  }

  function normalizeApplicationReceipt(value, resultId) {
    if (!plainObject(value)) return resultReceipt(resultId, "failed", "invalid_receipt");
    const allowed = new Set(["status", "code", "message", "receipt"]);
    if (!safeObjectKeys(value, allowed)) return resultReceipt(resultId, "failed", "invalid_receipt");
    if (!new Set(["applied", "rejected", "failed"]).has(value.status)) {
      return resultReceipt(resultId, "failed", "invalid_receipt");
    }
    const code = value.code === undefined ? "" : cleanExact(value.code, 64);
    if (code && !/^[a-z][a-z0-9_]{0,63}$/.test(code)) {
      return resultReceipt(resultId, "failed", "invalid_receipt");
    }
    const message = value.message === undefined ? "" : cleanExact(value.message, MAX_RECEIPT_STRING_LENGTH);
    if (value.message !== undefined && (!message || message !== value.message || /[\0\r\n]/.test(message))) {
      return resultReceipt(resultId, "failed", "invalid_receipt");
    }
    let receipt;
    if (value.receipt !== undefined) {
      receipt = cloneBoundedResultJson(value.receipt, {
        maxBytes: MAX_RECEIPT_BYTES,
        maxStringLength: MAX_RECEIPT_STRING_LENGTH,
        maxVisits: 2_048
      });
      if (receipt === INVALID_SEMANTIC_VALUE) {
        return resultReceipt(resultId, "failed", "invalid_receipt");
      }
    }
    return resultReceipt(resultId, value.status, code, message, receipt);
  }

  function resultReceipt(resultId, status, code = "", message = "", receipt = undefined) {
    const result = {
      contract_version: RESULT_RECEIPT_CONTRACT,
      result_id: resultId,
      status
    };
    if (code) result.code = code;
    if (message) result.message = message;
    if (receipt !== undefined) result.receipt = receipt;
    return result;
  }

  function cloneResultForApplication(result) {
    return {
      sink_id: result.sink_id,
      result_id: result.result_id,
      ...(result.expected_state_revision
        ? { expected_state_revision: result.expected_state_revision }
        : {}),
      payload: result.payload
    };
  }

  function resultMatchesCurrentPage(result) {
    const version = currentArtifactVersion();
    if (version <= 0 || version !== result.displayed_version) return false;
    let pathname = "";
    try {
      pathname = cleanExact(window.location && window.location.pathname, 1024);
    } catch {
      return false;
    }
    const segments = pathname.split("/").filter(Boolean);
    const artifactsIndex = segments.indexOf("artifacts");
    return artifactsIndex >= 0 && segments[artifactsIndex + 1] === result.artifact_id;
  }

  function cloneBoundedResultJson(value, limits) {
    const cloned = cloneResultJson(value, 0, new WeakSet(), { remaining: limits.maxVisits }, limits);
    if (cloned === INVALID_SEMANTIC_VALUE) return INVALID_SEMANTIC_VALUE;
    try {
      return jsonByteLength(cloned) <= limits.maxBytes ? cloned : INVALID_SEMANTIC_VALUE;
    } catch {
      return INVALID_SEMANTIC_VALUE;
    }
  }

  function cloneResultJson(value, depth, ancestors, visits, limits) {
    if (depth > MAX_RESULT_DEPTH || visits.remaining <= 0) return INVALID_SEMANTIC_VALUE;
    visits.remaining -= 1;
    if (value === null || typeof value === "boolean") return value;
    if (typeof value === "number") return Number.isFinite(value) ? value : INVALID_SEMANTIC_VALUE;
    if (typeof value === "string") {
      return semanticLengthAtMost(value, limits.maxStringLength) ? value : INVALID_SEMANTIC_VALUE;
    }
    if (!value || typeof value !== "object" || ancestors.has(value)) return INVALID_SEMANTIC_VALUE;
    ancestors.add(value);
    try {
      if (Array.isArray(value)) {
        if (value.length > MAX_RESULT_ARRAY_ITEMS) return INVALID_SEMANTIC_VALUE;
        const result = [];
        for (let index = 0; index < value.length; index += 1) {
          let child;
          try {
            child = value[index];
          } catch {
            return INVALID_SEMANTIC_VALUE;
          }
          const cloned = cloneResultJson(child, depth + 1, ancestors, visits, limits);
          if (cloned === INVALID_SEMANTIC_VALUE) return INVALID_SEMANTIC_VALUE;
          result.push(cloned);
        }
        return result;
      }
      if (!semanticPlainObject(value)) return INVALID_SEMANTIC_VALUE;
      let keys;
      try {
        keys = Object.keys(value);
      } catch {
        return INVALID_SEMANTIC_VALUE;
      }
      if (keys.length > MAX_RESULT_OBJECT_KEYS) return INVALID_SEMANTIC_VALUE;
      const result = {};
      for (const key of keys.sort()) {
        if (!semanticLengthAtMost(key, MAX_RESULT_KEY_LENGTH) || UNSAFE_SEMANTIC_KEYS.has(key)) {
          return INVALID_SEMANTIC_VALUE;
        }
        let child;
        try {
          child = value[key];
        } catch {
          return INVALID_SEMANTIC_VALUE;
        }
        const cloned = cloneResultJson(child, depth + 1, ancestors, visits, limits);
        if (cloned === INVALID_SEMANTIC_VALUE) return INVALID_SEMANTIC_VALUE;
        result[key] = cloned;
      }
      return result;
    } finally {
      ancestors.delete(value);
    }
  }

  function safeObjectKeys(value, allowed) {
    try {
      return Object.keys(value).every((key) => allowed.has(key) && !UNSAFE_SEMANTIC_KEYS.has(key));
    } catch {
      return false;
    }
  }

  function validArtifactId(value) {
    return typeof value === "string"
      && /^(?=.{1,64}$)[a-z0-9]+(?:[a-z0-9._-]*[a-z0-9])?$/.test(value);
  }

  function validResultSinkId(value) {
    return typeof value === "string" && value.length <= 128
      && /^[a-z][a-z0-9]*(?:[._-][a-z0-9]+)*\.v[1-9]\d*$/.test(value);
  }

  function validResultId(value) {
    return typeof value === "string" && /^arr_[A-Za-z0-9_-]{43}$/.test(value);
  }

  function validTaskId(value) {
    return typeof value === "string" && /^atk_[A-Za-z0-9_-]{43}$/.test(value);
  }

  function optionalResultRevision(value) {
    if (value === undefined || value === null || value === "") return null;
    return typeof value === "string" && value === value.trim() && value.length > 0
      && value.length <= 128 && !/[\0\r\n]/.test(value)
      ? value
      : false;
  }

  function collectContext() {
    const context = {
      contract_version: CONTEXT_CONTRACT,
      observed_at: new Date().toISOString()
    };
    const title = cleanText(document.title, 256);
    if (title) context.title = title;

    const pathname = cleanExact(window.location && window.location.pathname, 1024);
    const hash = cleanExact(window.location && window.location.hash, 512);
    if ((pathname && pathname.startsWith("/")) || (hash && hash.startsWith("#"))) {
      context.location = {};
      if (pathname && pathname.startsWith("/")) context.location.pathname = pathname;
      if (hash && hash.startsWith("#")) context.location.hash = hash;
    }

    const selectedText = currentSelection();
    if (selectedText) context.selected_text = selectedText;
    if (lastInteraction) context.last_interaction = { ...lastInteraction };

    const controls = collectControls();
    if (controls.length > 0) context.controls = controls;

    const semanticContext = collectSemanticContext();
    if (semanticContext !== INVALID_SEMANTIC_VALUE) context.semantic_context = semanticContext;
    const dirty = collectDirtyState(semanticContext);
    if (dirty !== null) context.dirty = dirty;
    const artifactVersion = currentArtifactVersion();
    if (artifactVersion > 0) context.artifact_version = artifactVersion;
    return context;
  }

  function collectDirtyState(semanticContext) {
    let api;
    try {
      api = window.catscoArtifact;
    } catch {
      return semanticDirtyState(semanticContext);
    }
    try {
      const isDirty = api && api.isDirty;
      if (typeof isDirty === "function") {
        const value = isDirty.call(api);
        if (typeof value === "boolean") return value;
      }
    } catch {}
    try {
      if (api && typeof api.dirty === "boolean") return api.dirty;
    } catch {}
    return semanticDirtyState(semanticContext);
  }

  function semanticDirtyState(value) {
    return semanticPlainObject(value) && typeof value.dirty === "boolean"
      ? value.dirty
      : null;
  }

  function currentArtifactVersion() {
    let search = "";
    let pathname = "";
    try {
      search = cleanExact(window.location && window.location.search, 1024);
      pathname = cleanExact(window.location && window.location.pathname, 1024);
    } catch {
      return 0;
    }
    const queryMatch = search.match(/(?:^|[?&])artifact_version=([1-9]\d*)(?:&|$)/);
    const pathMatch = pathname.match(/\/v([1-9]\d*)(?:\/|$)/);
    const parsed = Number(queryMatch?.[1] || pathMatch?.[1] || 0);
    return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : 0;
  }

  function collectSemanticContext() {
    const application = collectApplicationSemanticContext();
    if (!runtimeView) return application;
    const view = cloneRuntimeValue(runtimeView, MAX_RUNTIME_VIEW_BYTES);
    if (!plainObject(view)) return application;
    let combined;
    if (application === INVALID_SEMANTIC_VALUE) {
      combined = { runtime_view: view };
    } else if (semanticPlainObject(application)) {
      combined = { ...application, runtime_view: view };
    } else {
      combined = { application, runtime_view: view };
    }
    return jsonByteLength(combined) <= MAX_SEMANTIC_BYTES
      ? combined
      : { runtime_view: view };
  }

  function collectApplicationSemanticContext() {
    let api;
    try {
      api = window.catscoArtifact;
    } catch {
      return INVALID_SEMANTIC_VALUE;
    }
    let getContext;
    try {
      getContext = api && api.getContext;
    } catch {
      return INVALID_SEMANTIC_VALUE;
    }
    if (typeof getContext !== "function") return INVALID_SEMANTIC_VALUE;

    let value;
    try {
      value = getContext.call(api);
    } catch {
      return INVALID_SEMANTIC_VALUE;
    }
    try {
      const sanitized = sanitizeSemanticValue(value, 0, new WeakSet(), { remaining: MAX_SEMANTIC_VISITS });
      if (sanitized === INVALID_SEMANTIC_VALUE || !hasSemanticContent(sanitized)) {
        return INVALID_SEMANTIC_VALUE;
      }
      const size = jsonByteLength(sanitized);
      return size > 0 && size <= MAX_SEMANTIC_BYTES ? sanitized : INVALID_SEMANTIC_VALUE;
    } catch {
      return INVALID_SEMANTIC_VALUE;
    }
  }

  function sanitizeSemanticValue(value, depth, ancestors, visits) {
    if (depth > MAX_SEMANTIC_DEPTH || visits.remaining <= 0) return INVALID_SEMANTIC_VALUE;
    visits.remaining -= 1;
    if (value === null) return null;
    if (typeof value === "string") return truncateSemanticString(value, MAX_SEMANTIC_STRING_LENGTH);
    if (typeof value === "boolean") return value;
    if (typeof value === "number") return Number.isFinite(value) ? value : INVALID_SEMANTIC_VALUE;
    if (!value || typeof value !== "object" || ancestors.has(value)) return INVALID_SEMANTIC_VALUE;

    ancestors.add(value);
    try {
      if (Array.isArray(value)) {
        const result = [];
        const limit = Math.min(value.length, MAX_SEMANTIC_ARRAY_ITEMS);
        for (let index = 0; index < limit && visits.remaining > 0; index += 1) {
          let item;
          try {
            item = value[index];
          } catch {
            continue;
          }
          const sanitized = sanitizeSemanticValue(item, depth + 1, ancestors, visits);
          if (sanitized !== INVALID_SEMANTIC_VALUE) result.push(sanitized);
        }
        return result;
      }
      if (!semanticPlainObject(value)) return INVALID_SEMANTIC_VALUE;

      const result = {};
      let keys;
      try {
        keys = [];
        for (const key in value) {
          if (!Object.prototype.hasOwnProperty.call(value, key)) continue;
          if (!semanticLengthAtMost(key, MAX_SEMANTIC_KEY_LENGTH) || UNSAFE_SEMANTIC_KEYS.has(key)) continue;
          keys.push(key);
          if (keys.length >= MAX_SEMANTIC_OBJECT_KEYS) break;
        }
        keys.sort();
      } catch {
        return INVALID_SEMANTIC_VALUE;
      }
      for (const key of keys) {
        if (visits.remaining <= 0) break;
        let child;
        try {
          child = value[key];
        } catch {
          continue;
        }
        const sanitized = sanitizeSemanticValue(child, depth + 1, ancestors, visits);
        if (sanitized !== INVALID_SEMANTIC_VALUE) result[key] = sanitized;
      }
      return result;
    } finally {
      ancestors.delete(value);
    }
  }

  function semanticPlainObject(value) {
    try {
      const prototype = Object.getPrototypeOf(value);
      if (prototype === null || prototype === Object.prototype) return true;
      if (Object.getPrototypeOf(prototype) !== null) return false;
      const descriptor = Object.getOwnPropertyDescriptor(prototype, "constructor");
      return Boolean(descriptor)
        && typeof descriptor.value === "function"
        && Function.prototype.toString.call(descriptor.value) === "function Object() { [native code] }";
    } catch {
      return false;
    }
  }

  function truncateSemanticString(value, limit) {
    let count = 0;
    let end = 0;
    for (const character of value) {
      if (count >= limit) break;
      count += 1;
      end += character.length;
    }
    return value.slice(0, end);
  }

  function semanticLengthAtMost(value, limit) {
    let count = 0;
    for (const unused of value) {
      void unused;
      count += 1;
      if (count > limit) return false;
    }
    return true;
  }

  function hasSemanticContent(value) {
    if (value === null) return false;
    if (typeof value === "string") return value.length > 0;
    if (typeof value === "boolean" || typeof value === "number") return true;
    if (Array.isArray(value)) return value.length > 0;
    return semanticPlainObject(value) && Object.keys(value).length > 0;
  }

  function jsonByteLength(value) {
    let text;
    try {
      text = JSON.stringify(value);
    } catch {
      return 0;
    }
    if (typeof text !== "string") return 0;
    let length = 0;
    for (let index = 0; index < text.length; index += 1) {
      const code = text.charCodeAt(index);
      if (code <= 0x7f) length += 1;
      else if (code <= 0x7ff) length += 2;
      else if (code >= 0xd800 && code <= 0xdbff && index + 1 < text.length
        && text.charCodeAt(index + 1) >= 0xdc00 && text.charCodeAt(index + 1) <= 0xdfff) {
        length += 4;
        index += 1;
      } else length += 3;
    }
    return length;
  }

  function rememberInteraction(event) {
    const target = event && event.target;
    if (!elementLike(target) || !visibleElement(target)) return;
    if (event.isTrusted === true) {
      lastTrustedTaskActivationAt = Date.now();
      taskActivationSerial += 1;
    }
    lastInteraction = interactionSummary(target);
    if (controlType(target)) lastControl = target;
  }

  function consumeTaskUserActivation() {
    if (taskActivationSerial <= consumedTaskActivationSerial || !lastTrustedTaskActivationAt
        || Date.now() - lastTrustedTaskActivationAt > TASK_USER_ACTIVATION_MAX_AGE_MS) return false;
    consumedTaskActivationSerial = taskActivationSerial;
    return true;
  }

  function interactionSummary(element) {
    const summary = {};
    const tag = cleanText(element.tagName, 32).toLowerCase();
    if (/^[a-z][a-z0-9-]*$/.test(tag)) summary.tag = tag;
    const role = cleanText(attribute(element, "role"), 64);
    const name = cleanText(element.name || attribute(element, "name") || attribute(element, "aria-label"), 256);
    const text = cleanText(element.innerText || element.textContent || element.value, 256);
    if (role) summary.role = role;
    if (name) summary.name = name;
    if (text) summary.text = text;
    return Object.keys(summary).length > 0 ? summary : null;
  }

  function collectControls() {
    const candidates = [];
    const seen = new Set();
    const add = (element) => {
      if (!elementLike(element) || seen.has(element) || !visibleElement(element)) return;
      seen.add(element);
      candidates.push(element);
    };
    try {
      for (const element of document.querySelectorAll("input:checked, select")) add(element);
    } catch {}
    add(lastControl);

    const controls = [];
    for (const element of candidates) {
      const summary = controlSummary(element);
      if (summary) controls.push(summary);
      if (controls.length >= MAX_CONTROLS) break;
    }
    return controls;
  }

  function controlSummary(element) {
    const type = controlType(element);
    if (!type || !CONTROL_TYPES.has(type)) return null;
    if ((type === "checkbox" || type === "radio") && element.checked !== true) return null;
    const summary = { type };
    const name = cleanText(element.name || attribute(element, "name"), 256);
    const ariaLabel = cleanText(attribute(element, "aria-label"), 256);
    const role = cleanText(attribute(element, "role"), 64);
    const value = cleanText(element.value, 512);
    const text = type.startsWith("select-")
      ? cleanText(selectedOptionText(element), 256)
      : cleanText(element.innerText || element.textContent, 256);
    if (name) summary.name = name;
    if (ariaLabel) summary.aria_label = ariaLabel;
    if (role) summary.role = role;
    if (value) summary.value = value;
    if (text) summary.text = text;
    if (type === "checkbox" || type === "radio") summary.checked = true;
    return Object.keys(summary).length > 1 ? summary : null;
  }

  function controlType(element) {
    const tag = cleanText(element && element.tagName, 32).toLowerCase();
    if (tag === "textarea") return "textarea";
    if (tag === "select") return element.multiple ? "select-multiple" : "select-one";
    if (tag !== "input") return "";
    const type = cleanText(element.type || attribute(element, "type") || "text", 32).toLowerCase();
    return CONTROL_TYPES.has(type) ? type : "";
  }

  function selectedOptionText(element) {
    const options = element && element.selectedOptions;
    if (!options || typeof options[Symbol.iterator] !== "function") return "";
    return Array.from(options).map((option) => cleanText(option.textContent, 128)).filter(Boolean).join(", ");
  }

  function currentSelection() {
    try {
      return cleanText(window.getSelection && window.getSelection().toString(), 2000);
    } catch {
      return "";
    }
  }

  function visibleElement(element) {
    if (!elementLike(element) || element.hidden || attribute(element, "aria-hidden") === "true") return false;
    const type = cleanText(element.type || attribute(element, "type"), 32).toLowerCase();
    if (type === "password" || type === "file" || type === "hidden") return false;
    try {
      const style = window.getComputedStyle && window.getComputedStyle(element);
      if (style && (style.display === "none" || style.visibility === "hidden")) return false;
    } catch {}
    return true;
  }

  function allowedParentOrigin(origin) {
    try {
      const parsed = new URL(origin);
      if (parsed.protocol === "https:" && (!parsed.port || parsed.port === "443")) {
        if (TRUSTED_PARENT_HOSTS.has(parsed.hostname)) return true;
        for (const suffix of TRUSTED_PARENT_SUFFIXES) {
          if (parsed.hostname.endsWith(suffix)) return true;
        }
      }
      return (parsed.hostname === "localhost" || parsed.hostname === "127.0.0.1")
        && (parsed.protocol === "http:" || parsed.protocol === "https:");
    } catch {
      return false;
    }
  }

  function validRequestId(value) {
    return typeof value === "string" && value.length > 0 && value.length <= 128
      && /^[A-Za-z0-9._:-]+$/.test(value);
  }

  function elementLike(value) {
    return value && typeof value === "object" && typeof value.tagName === "string";
  }

  function plainObject(value) {
    return value && typeof value === "object" && !Array.isArray(value);
  }

  function attribute(element, name) {
    try {
      return element && typeof element.getAttribute === "function" ? element.getAttribute(name) : "";
    } catch {
      return "";
    }
  }

  function cleanText(value, limit) {
    return cleanExact(value, limit).trim();
  }

  function cleanExact(value, limit) {
    if (typeof value !== "string") return "";
    return value.length <= limit ? value : value.slice(0, limit);
  }
})();
