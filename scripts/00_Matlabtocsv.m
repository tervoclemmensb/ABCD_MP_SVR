run("/Volumes/Zeus/Finn/ABCD/abcd_cca/cca/setupAndLoadVars.m")

d_g1=array2table(r);
d_g1.subj=g1_subjects;

d_g2=array2table(r_rep);
d_g2.subj=g2_subjects;


writetable(d_g1,"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.disc.csv");
writetable(d_g2,"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.rep.csv");

%% behavior vars%%%

abcd_discovery = load('/Volumes/Zeus/Finn/ABCD/abcd_cca/cca/MainTable_g1.mat');
%%%everything but nested matrices
keepidx=setdiff(1:width(abcd_discovery.MainTable_g1),[ 2 8]);
writetable(abcd_discovery.MainTable_g1(:,keepidx), '/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.disc.csv');

abcd_replication = load('/Volumes/Zeus/Finn/ABCD/abcd_cca/cca/MainTable_g2.mat');
keepidx=setdiff(1:width(abcd_replication.MainTable_g2),[ 2 8]);
writetable(abcd_replication.MainTable_g2(:,keepidx), '/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.rep.csv');


