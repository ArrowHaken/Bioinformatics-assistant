/share/apps/R/R-4.1.2-4/bin/Rscript /share/home/liny/Project/personality/script/markgene/markgene.R --rds /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds --outdir /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/2.markgene --reduction umap --species Mouse
/share/apps/R/R-4.1.2-4/bin/Rscript markgene_celltype.R --rds /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds --outdir /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/2.markgene --reduction umap --species Mouse
#cd /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/2.markgene
#cat sig_markgene.xls|cut -f 6|uniq|sed '1d'|while read a;do mkdir 'cluster'$a;done
#ls -d cluster*/|awk -F '/'  '{split($1,l,"cluster");print "awk -F \"\\t\" \"{if(NR>1){if(\\$6==\\\""l[2]"\\\")print}else{print}}\" sig_markgene.xls >"$0$1"_sig_markgene.xls"}' |sh
#ls -d cluster*/|awk -F '/' '{print "cp /share/home/liny/Project/personality/script/markgene/cluster_profile_mmu.R "$0"cluster_profile.R\nsed -i \"s#cluster0#"$1"#g\" "$0"cluster_profile.R"}'|sh
#rm run.sh
#ls -d cluster*/|awk -F '/' '{print "cd /share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/2.markgene/"$1"\n/share/apps/R/R-4.1.2-4/bin/Rscript cluster_profile.R"}'>>run.sh
#sh run.sh
