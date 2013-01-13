function [  ] = matlab2fortran( varargin )

% example 1:
% matlab2fortran('tests/test1.m');
% matlab2fortran('tests/test1.m','tests/test2.m');

% example 2: all files in current directory (does not work with subfolders with dir..)
% FileList=dir('*.m');
% matlab2fortran(FileList.name);

% Params
bDebug=0;% show input in stdout
bPipe=0; % pipe output to stdout

if nargin==0
    error('Empty file list')
end

%% dealing with multiple input files "recursively..."
if length(varargin)>1
    for ifile = 1:length(varargin)
        f=varargin{ifile};
        matlab2fortran(f);
    end
else
    file=varargin{1};
    fidm=fopen(file);
    if fidm==-1
        error(['Unable to open input file: ' file]);
    end


    filef=regexprep(file,'\.m$','.f90');
    fprintf('Converting to file: %s\n',filef);
    %     warning off
    if ~bDebug
        fidf=fopen(filef,'w');
        if fidf==-1
            error(['Unable to open ouput file: ' file]);
        end
    else
        fidf=1; % standard output 
    end

    fconvertlinebyline(fidm,fidf,bDebug,bPipe)
    fclose(fidm);
    if ~bDebug 
        fclose(fidf);
    end
end
end %function

function []=fconvertlinebyline(fidm,fidf,bDebug,bPipe)
sLine = fgets(fidm);
end_stack=[];
pref_in='';
pref_out='';
if bDebug 
    pref_in='In :';
    pref_out='Out:';
end
while sLine ~= -1
    % remove useless spaces 
    sLine=strtrim(sLine);
    sLine=fremovespaces(sLine);

    if bDebug 
        fprintf('%s%s \n',pref_in,sLine);
    end

    % Simple case: start by a comment, replace it
    if isempty(sLine)
        sf=[sLine ' \n'];
        fprintf(fidf,[pref_out,sf]);
        if bPipe ; fprintf(1,[pref_out,sf]); end
%     elseif sLine(1)=='%'
%         sLine(1)='!';
%         sf=[sLine ' \n'];
%         disp(sf)
%         fprintf(fidf,[pref_out,sf]);
%         if bPipe ; fprintf(1,[pref_out,sf]); end
     else
        % the problematic printf case is dealt then
        if ~isempty(regexp(sLine,'printf\(')) 
            [ sf ] = freplaceprintf( sLine );
            fprintf(fidf,[pref_out,sf]);
            if bPipe ; fprintf(1,[pref_out,sf]); end
        else
            % comments replacement
            sLine=strrep(sLine,'%','!');
            % splitting matlab lines if commands on the same line
            [ sout ] = fsplitmatlablines( sLine,[] );
            for isplit=1:length(sout)
                s=sout{isplit};
                % ---------------------------------
                % The Main function
                % ---------------------------------
                [sf end_stack]=fconvertline(s,end_stack);
                % ---------------------------------
                sf=fremovespaces(sf);
                fprintf(fidf,[pref_out,sf]);
                if bPipe ; fprintf(1,[pref_out,sf]); end
            end
        end
    end
    sLine = fgets(fidm);
end

% if at the end we still have some end, output them
for ie=1:length(end_stack)
    fprintf(fidf,'end %s \n',end_stack{ie});
end

end

function [sf end_stack]=fconvertline(s,end_stack)
% function replace
I=strfind(s,'function');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacefunction( s , end_stack );
    return;
end

%% Simple regexprep or rep
% logical
s=regexprep(s,'~=','/=');
s=regexprep(s,'~(.*)','.not.($1)');
s=strrep(s,'&&','.and.');
s=strrep(s,'||','.or.');
s=strrep(s,'break','exit');
s=strrep(s,'true','.true.');
s=strrep(s,'false','.false.');

% intrisic
s=strrep(s,'strtrim(','trim(');
s=strrep(s,'ceil(','ceiling(');
s=strrep(s,'round(','int(');
s=strrep(s,'floor(','floor(');
s=strrep(s,'mod(','modulo('); %mod exists but return negative values
% array stuff
s=regexprep(s,'size\(([^,]*)\)','shape($1)'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'length(','size('); % or count...

% math
s=strrep(s,'dot','dot_product'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'^','**'); 

% strings
s=strrep(s,'\','\\'); 

%% Things that are easily recognizable by their first position on the line
% case
s=regexprep(s,'^case([^!;]*)','case ($1) !');
s=regexprep(s,'^otherwise*)','case default ');
sold=s;
s=regexprep(sold,'^switch([^!;]*)','select case ($1) !');
if length(s)~=length(sold) % we did replace something
    end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
    end_stack{1}='select';
end

% for replace
I=strfind(s,'for');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacefor( s , end_stack );
    return;
end
% while replace
I=strfind(s,'while');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacewhile( s , end_stack );
    return;
end

% if replace
I=regexp(s,'if\ |if\(|elseif|else\ if');
if ~isempty(I) &&  min(I)==1
    [ sf end_stack] = freplaceif( s , end_stack );
    return;
end

% end replace
I=strfind(s,'end');
if ~isempty(I) && I(1)==1  
    %let's check that this is not a variable
    if length(s)==3 || s(4)==' ' || s(4)==';' ||s(4)=='!'
        [ sf end_stack] = freplaceend( s , end_stack );
        return;
    end
end

% function calls
I=strfind(s,'[');
if ~isempty(I) && I(1)==1 
    [ sf] = freplacefunctioncall( s );
    return;
end

% brackets
s=strrep(s,'[','(/');
s=strrep(s,']','/)');
% multiline
s=strrep(s,'...',''); %char(8) not enough..


%% More tricky things that should require syntax parsing and recursion but that we'll not do!..
% zeros and ones
I=regexp(s,'(zeros\(|ones\(|linspace\()');
if ~isempty(I)
    [ s] = freplacezeros( s );
end



% default return
sf=[s '\n'];
end

