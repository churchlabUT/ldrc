
[path file] = textread('scrubbinglist_5.txt','%s%s');
for i = 1:length(path);
    censor = load([path{i} file{i}]);
    onecolumn = sum(censor,2);
    onecolumn(find(onecolumn==1))=2;
    onecolumn(find(onecolumn==0))=1;
    onecolumn(find(onecolumn==2))=0;
    dlmwrite([path{i} '/' 'FIDL_scrub_FD09_0f0b.txt'],onecolumn)
end


%censor = load('fd_confounds.txt');
%onecolumn = sum(censor,2);
%onecolumn(find(onecolumn==1))=2;
%onecolumn(find(onecolumn==0))=1;
%onecolumn(find(onecolumn==2))=0;
%dlmwrite([path{i} '/' 'FIDL_scrub_FD09_0f0b.txt',onecolumn)