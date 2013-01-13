function [ s , vtype,ncol,nlines] = freplacesbracket( s);
% [ s , var1,ncol,nlines] = freplacesbracket( 'aaaa=[]!' );
% 
%
%
vtype='';
ncol=0;
nlines=0;

[b po pc]=ffindmatching(s,1,'[',']',1);
if b
    % pre and post strings without bracket
   spre=s(1:(po-1));
   spost=strtrim(s((pc+1):end));
    % inside bracket string
   sb=strtrim(s((po+1):(pc-1)));
   napostroph=length(strfind(sb,''''));
   if napostroph>0 && mod(napostroph,2)==0
       % deal with string, in an ugly way
       sb=strtrim(fremovespaces(sb));
       is=1;
       bwithin=0;
       while is<=length(sb)
           if sb(is)==''''
              if ~bwithin
                   bwithin=1;
               else
                   bwithin=0;
               end
           end
           if (sb(is)==' ' || sb(is)==',' || sb(is)==';')   && ~bwithin
               % concatenate strings
               sb=[sb(1:(is)) '// ' sb((is+1):end)];
               is=is+4;
           else
               is=is+1;
           end
       end
    vtype='character(len=*)';


   else
       % matching brackets have been found.. Assumed not including furtger brackets TODO
       % some really ugly stuff below to accept spaces, commas and semi-column as separator. A good regexp would help
       sb=fremovespaces(strtrim(sb)); % remove double spaces
       
       sb=strrep(sb,'; ', ';'); % 
       sb=strrep(sb,' ;', ';'); % 
       
       sb=strrep(sb,',', ' '); % replacing commas
       sb=fremovespaces(strtrim(sb)); % remove double spaces again
       % Now spaces have meaning, so we replace them by ,
       sb=strrep(sb,' ', ','); % replacing spaces
        
       nc=length(strfind(sb,','));
       nsc=length(strfind(sb,';'));
       nlines=nsc+1;
%      if nlines==1
           ncol=floor(nc/nlines)+1;
%        else
%            ncol= 
%        end
       if min(ncol,nlines)==1
           % no need to reshape, I think...
           sb=sprintf('[ %s ] ',strrep(sb,';', ','));
       else
           sb=sprintf('reshape([ %s ] , [ %d , %d] )',strrep(sb,';', ','),ncol,nlines);
       end

   end

   s=[spre sb spost];

end


end

