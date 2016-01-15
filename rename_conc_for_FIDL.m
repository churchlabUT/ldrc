o=pwd;
[path file num label] = textread('relabel_3.txt','%s%s%s%s');
for i = 1:length(path);
    cd([path{i}]);
    movefile([file{i}],[label{i} '_' num{i} file{i}]);
    cd(o);
end


%censor = load('fd_confounds.txt');
%onecolumn = sum(censor,2);
%onecolumn(find(onecolumn==1))=2;
%onecolumn(find(onecolumn==0))=1;
%onecolumn(find(onecolumn==2))=0;
%dlmwrite([path{i} '/' 'FIDL_scrub_FD09_0f0b.txt',onecolumn)