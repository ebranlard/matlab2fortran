function [ sout ] = fsplitmatlablines( s,sout )
% recursive function that splits s into
% s=fsplitmatlablines('',[])
% s=fsplitmatlablines('a',[])
% s=fsplitmatlablines('a;',[])
% s=fsplitmatlablines('a ;',[])
% s=fsplitmatlablines('a ; bhjhj',[])
% s=fsplitmatlablines('a ; bhjhj ; sdifsidfj ;',[])
% s=fsplitmatlablines('a ; bhjhj '';'' sdifsidfj ;',[])


% ifound=strfind(s,';')
ifound=regexp(s,'[;]'); % the case with , requires more care and is dropped for now..
% ifound=regexp(s,'[;,]');
if ~isempty(ifound)
    notmeaningful=1;
    itry=1;
    while notmeaningful && itry<=length(ifound)
        if s(ifound(itry))==';' 
            [b1,p1,p3]=fissurroundedby(s,ifound(itry),'''','''');
            [b2,p1,p3]=fissurroundedby(s,ifound(itry),'[',']');
            b=b1+b2>0;
            % dealing with the case where what follows is a comment.. this is nasty but well
            if ~b
                ii=ifound(itry);
                foundacomment=0;
                while ii<=length(s) && ~foundacomment;
                    if s(ii)=='!'
                        foundacomment=1;
                        break
                    end
                    if s(ii)==' ' || s(ii)==';'
                        ii=ii+1;
                    else
                        break
                    end
                end
                if foundacomment
                    s(ifound(itry))=' '; % removing the ';'
                    break
                end
            end
        else
            [b1]=fissurroundedby(s,ifound(itry),'[',']');
            [b2]=fissurroundedby(s,ifound(itry),'(',')');
            [b3]=fissurroundedby(s,ifound(itry),'''','''');
            b=b1+b2+b3>0;
        end
        if ~b
            notmeaningful=0;
            isplit=ifound(itry);
        end
        itry=itry+1;
    end
    if ~notmeaningful
        sout{end+1}=strtrim(s(1:(isplit-1)));
        if itry<length(s);
            send=strtrim(s((isplit+1):length(s)));
            if ~isempty(send)
                sout=fsplitmatlablines(send,sout);
            end
        end
    else
        sout{end+1}=s;
    end

else
    sout{end+1}=s;
end
end

