

%% initialise constants


DETECT_TRESHOLDS_MODE=false
ONLY_CONTROL_WELLS=DETECT_TRESHOLDS_MODE



LETTERS={'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P'};


%inputFolder='/home/feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/reconstruated_plates_processing/Morphology-project-2018/tiff_images_of_control_strains_for_Reka/170623_morphology_plate5_m46__2017-06-23T13_52_22-Measurement1/';
%inputBigFolder0='/mnt/hdd-1/space-of-feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/reconstruated_plates_processing/Morphology-project-2018/TMP_BIG_DATA/01-Operetta/';
inputBigFolder0='/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/01-Operetta/';

% Ebbe a ket forlderba teszi a kimenet. 
% Az outputBigFolder0 folder-be azt ami kell a CalMorph bemenetére.
% Az outputBigFolder_debug folder-be minden mást, ami a folyamat
% ellenőrzéséhez és követéséhez kell
outputBigFolder_debug='/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-image_processing_deteals-2021_04_26/';
outputBigFolder0='/home/feketeg/mounted-shares/raid-of-group/PROJECTS/YeastMorphology-2015-FarkasZoli_KovacsKaroly/DATA_of_microscope/02-Operetta_Images_newMatlab_Script-2021_04_26/';
mkdir(outputBigFolder0)
mkdir(outputBigFolder_debug)


outputFolder='/mnt/hdd-1/space-of-feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/natural_isolates_processing/Morphology-project-2018/TMP_BIG_DATA/my_matlab_out1/';
mkdir(outputFolder)

if DETECT_TRESHOLDS_MODE
    outputFileID = fopen(fullfile(outputBigFolder0,'treshold_tables_based_on_control_well_images.csv'),'w');
    fprintf(outputFileID,'YMP_folder_name,wvp,wellName,treshold_low_conA,median_conA,treshold_high_conA,treshold_low_dapi, median_dapi, treshold_high_dapi\n');
else
    
    outputFileID2 = fopen(fullfile(outputBigFolder0,'tif_jpg_filename_map.csv'),'w');
    fprintf(outputFileID2,'plate,well,viewport,image_number\n');
    
end

control_well_table=readtable('/mnt/hdd-1/space-of-feketeg/eclipse-workspaces/ws3-forEclipseOxigen/MORPHOLOGY-2018/natural_isolates_processing/Morphology-project-2018/TMP_BIG_DATA/control_well_list.csv', 'delimiter',',');



IMAGE_WIDTH=1360;
IMAGE_HEIGHT=1024;

OUT_IMAGE_WIDTH=696;
OUT_IMAGE_HEIGHT=520;


OUT_IMG_BOUNDARIES = ...
    [1                                  OUT_IMAGE_WIDTH            1                                  OUT_IMAGE_HEIGHT;...
    IMAGE_WIDTH-OUT_IMAGE_WIDTH+1      IMAGE_WIDTH                1                                  OUT_IMAGE_HEIGHT;...
    1                                  OUT_IMAGE_WIDTH            IMAGE_HEIGHT-OUT_IMAGE_HEIGHT+1    IMAGE_HEIGHT;...
    IMAGE_WIDTH-OUT_IMAGE_WIDTH+1      IMAGE_WIDTH                IMAGE_HEIGHT-OUT_IMAGE_HEIGHT+1    IMAGE_HEIGHT];




R=15; % radius of SD image filter
SD_TRESHOLD_conA=100;
SD_TRESHOLD_dapi=50;
[xx,yy] = ndgrid(1:(2*R+1),1:(2*R+1));
disk_mask = uint8(( (xx-R-1).^2 + (yy-R-1).^2)< (R+0.5)^2);
clearvars xx yy R


%%

if(~DETECT_TRESHOLDS_MODE)
    plate_treshold_table = readtable(fullfile(outputBigFolder0 ,'aggregated-plate-treshold.csv'));
end

%%

dirList1=dir(inputBigFolder0);
dirList1=dirList1([dirList1.isdir] & ~strcmp({dirList1.name},'.') & ~strcmp({dirList1.name},'..'));


% leszedem a per jeleket a konvónytranavek vegerol , ha van
list_of_dirs_in_xxx_file=cellfun(@(x) regexp(x, '^(.*[^\\/])\\?/?$','tokens'),control_well_table.YMP_folder_name, 'UniformOutput', true);
list_of_dirs_in_xxx_file=[list_of_dirs_in_xxx_file{:}];

% csak azokat a konytrakat veszem, amik emlitve vannak a control well-es
% csv fileban
dirList1=dirList1(ismember({dirList1.name},list_of_dirs_in_xxx_file));


%DEBUG i_dir1=1
for i_dir1=1:length(dirList1)
    fprintf('%s\n', dirList1(i_dir1).name);
    
    
    plateFolderName= dirList1(i_dir1).name;
    inputFolder= fullfile(dirList1(i_dir1).folder, plateFolderName,'Images');
    
    if ~DETECT_TRESHOLDS_MODE
        outputFolder_debug=fullfile(outputBigFolder_debug, plateFolderName);
        outputFolder0=fullfile(outputBigFolder0, plateFolderName);
        
        mkdir(outputFolder0);
        mkdir(outputFolder_debug);
    end
    
    
    if(ONLY_CONTROL_WELLS)
        % kiszedem a control wellek nevit a tablazatbol
        idx1=strcmp( control_well_table.YMP_folder_name, dirList1(i_dir1).name);
        control_wells_on_the_actual_plate=control_well_table.YMP_well( idx1 );
        clearvars idx1
    end
    
    %% betoltom a konytraban levo fileneveket. Kiszurom azokat amikre szuksegem van
    
    filelist1=dir(inputFolder);
    
    %% ellenorzom, h az osszetartozo dapi-conA kepek parban vannak-e
    
    % regexp ... tartalmazza-e a ch3 infix-et . Az a ConA chanel
    idx1 = cellfun(@(x) ~isempty(regexp(x, 'ch3')), {filelist1.name}, 'UniformOutput', true);
    listOfImages_conAChanel = filelist1(idx1);
    
    idx1 = cellfun(@(x) ~isempty(regexp(x, 'ch2')), {filelist1.name}, 'UniformOutput', true);
    listOfImages_dapiChanel = filelist1(idx1);
    
    % ellenorzom, hogy ugyanazok a kepek megvannak conA es dapi chanelen is
    assert(  isequal(sort(strrep({listOfImages_conAChanel.name},'ch3','ch2')) ,sort({listOfImages_dapiChanel.name})) );
    
    clearvars listOfImages_conAChanel listOfImages_dapiChanel idx1
    
    
    
    
    %%
    
    %     tmp=cellfun(@(x) regexp(x, '^(?<position>r\d\dc\d\df\d\d).*\.tiff$','tokens'),{filelist1.name}, 'UniformOutput', false);
    %     idx1= cellfun(@length,tmp) ==1;
    %     filelist1=filelist1(idx1);
    %     tmp=tmp(idx1);
    %
    % tmp=cellfun(@(x) x{1},tmp);
    %     well_and_vp_list=unique( tmp);  % well position an wiewport
    %     clearvars idx1 tmp
    %
    
    tmp=cellfun(@(x) regexp(x, '^(?<position>r\d\dc\d\d)f\d\d.*\.tiff$','tokens'),{filelist1.name}, 'UniformOutput', false);
    idx1= cellfun(@length,tmp) ==1;
    filelist1=filelist1(idx1);
    tmp=tmp(idx1);
    
    tmp=cellfun(@(x) x{1},tmp);
    well_list=unique( tmp);  % well position an wiewport
    clearvars idx1 tmp
    
    %DEBUG  i_well=1
    for i_well = 1:length(well_list)
        wellPositionName=well_list{i_well};
        
        tmp=regexp(wellPositionName,'^r(?<row>\d\d)c(?<col>\d\d)$','names');
        assert(length(tmp)==1); % volt regexp illeszkedes
        row=str2num(tmp.row);
        column=str2num(tmp.col);
        wellName = sprintf('%s%0.2d',row+'A'-1 , column);
        
        
        fprintf('  %s\n', wellName);
        
        
        
        if ~DETECT_TRESHOLDS_MODE
            outputFolder_debug1=fullfile(outputFolder_debug, wellName);
            outputFolder1=fullfile(outputFolder0, wellName);
            
            
            if ~exist(outputFolder1, 'dir')
                mkdir(outputFolder1)
            end
            
            if ~exist(outputFolder_debug1, 'dir')
                mkdir(outputFolder_debug1)
            end
            
            out_file_counter=1;
        end
        
        my_regexp_1=sprintf('^(?<position>%sf\\d\\d).*\\.tiff$',wellPositionName);
        tmp=cellfun(@(x) regexp(x, my_regexp_1,'tokens'),{filelist1.name}, 'UniformOutput', false);
        idx1= cellfun(@length,tmp) ==1;
        %filelist1=filelist1(idx1);
        tmp=tmp(idx1);
        
        tmp=cellfun(@(x) x{1},tmp);
        well_and_vp_list=unique( tmp);  % well position an wiewport
        clearvars idx1 tmp
        
        
        
        
        
        %DEBUG i_well_and_vp=1
        for i_well_and_vp = 1:length(well_and_vp_list)
            wvp=well_and_vp_list{i_well_and_vp};
            
            
            
            % az if feltetel masodik felel lehet, h ki sem ertekelodik... nem
            % is tudna
            if ~ONLY_CONTROL_WELLS || ismember(wellName, control_wells_on_the_actual_plate)
                
                % keresem conA chanel kepeit
                regexp_expr=sprintf('^%sp\\d\\d-ch3.*$', wvp);
                idx1 = cellfun(@(x) ~isempty(regexp(x, regexp_expr)), {filelist1.name}, 'UniformOutput', true);
                listOfImages_wp_conA = filelist1(idx1);
                
                % rendezem nev szerint biztos ami biztos
                % a focus index kepeknel ez fontos lesz
                [~, idx_order]=   sort({ listOfImages_wp_conA.name});
                listOfImages_wp_conA=listOfImages_wp_conA(idx_order);
                
                
                %% Betoltom a kepeket
                
                % betoltom az osszes ugyanabba a z-stack-be tartozo kepet a conA es a dapi
                % chanelen is. Beteszem struct array-ba
                image_table=struct();
                for i_zStack=1:length(listOfImages_wp_conA)
                    file1= listOfImages_wp_conA(i_zStack);
                    image1=imread(fullfile(file1.folder,file1.name));
                    assert(  all(size(image1)==[IMAGE_HEIGHT IMAGE_WIDTH]));
                    
                    image2=imread(fullfile(file1.folder,strrep(file1.name,'ch3','ch2')));
                    assert(  all(size(image2)==[IMAGE_HEIGHT IMAGE_WIDTH]));
                    
                    
                    image_table.image_conA(i_zStack)={image1};
                    image_table.image_dapi(i_zStack)={image2};
                    image_table.name(i_zStack)={file1.name};
                end
                
                
                %% a z-stack-ben levo kepeknek veszem a maximumat oixelenkent
                image_MaxValues_conA=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,"uint16");
                image_MaxValues_dapi=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,"uint16");
                
                for i_zStack=1:length(listOfImages_wp_conA)
                    image_conA=image_table.image_conA{i_zStack};
                    image_dapi=image_table.image_dapi{i_zStack};
                    
                    image_MaxValues_conA=max(image_MaxValues_conA,image_conA);
                    image_MaxValues_dapi=max(image_MaxValues_dapi,image_dapi);
                end
                clearvars  image_dapi image_conA
                
                %%
                
                if DETECT_TRESHOLDS_MODE
                    %% Csinalok kombinalt mask-ot ami kijelöli a hatteret (hatter az, aminek kicsi a szorasa mindket chanel-en)
                    
                    image_mask_dapi=false(IMAGE_HEIGHT, IMAGE_WIDTH);
                    image_mask=false(IMAGE_HEIGHT, IMAGE_WIDTH);
                    
                    for i_zStack=1:length(listOfImages_wp_conA)
                        image_conA=image_table.image_conA{i_zStack};
                        image_dapi=image_table.image_dapi{i_zStack};
                        
                        image_SD=stdfilt(image_conA,disk_mask);
                        image_mask_0=image_SD>SD_TRESHOLD_conA ;  % 1 ~ sejt  ;  0 ~ hatter
                        image_mask=or(image_mask_0,image_mask);
                        
                        image_SD=stdfilt(image_dapi,disk_mask);
                        image_mask_0=image_SD>SD_TRESHOLD_dapi ;  % 1 ~ sejt  ;  0 ~ hatter
                        image_mask=or(image_SD>SD_TRESHOLD_dapi,image_mask);
                        
                        image_mask_dapi=or(image_mask_0,image_mask_dapi);
                        
                    end
                    
                    
                    clearvars image_mask_0 image_dapi image_conA image_SD
                    
                    %% tresholdok kialakitasa
                    treshold_low_conA = quantile(image_MaxValues_conA(~image_mask),0.999);
                    treshold_low_dapi = quantile(image_MaxValues_dapi(~image_mask),0.999);
                    
                    tmp=image_MaxValues_conA(image_MaxValues_conA>treshold_low_conA);
                    median_conA=quantile(tmp,0.5);
                    treshold_high_conA = treshold_low_conA+12*(median_conA-treshold_low_conA);
                    
                    tmp=image_MaxValues_dapi(image_MaxValues_dapi>treshold_low_dapi);
                    median_dapi=quantile(tmp,0.5);
                    treshold_high_dapi = treshold_low_dapi+12*(median_dapi-treshold_low_dapi);
                    
                    fprintf("\nConA TR_low=%i, median=%i, TR_high=%i\ndapi TR_low=%i, median=%i, TR_high=%i\n", ...
                        treshold_low_conA,median_conA,treshold_high_conA,...
                        treshold_low_dapi, median_dapi, treshold_high_dapi);
                    
                    fprintf(outputFileID,'%s,%s,%s,%i,%i,%i,%i,%i,%i\n',...
                        dirList1(i_dir1).name,wvp, wellName,...
                        treshold_low_conA,median_conA,treshold_high_conA,...
                        treshold_low_dapi, median_dapi, treshold_high_dapi);
                    
                    %     %%  hisztogramok rajzolasa
                    %     tiledlayout(3,2)
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_conA(image_mask))
                    %     xline(treshold_low_conA,'--r')
                    %     xline(median_conA,'--r')
                    %     xline(treshold_high_conA,'--r')
                    %     title("conA - on the cells")
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_conA(~image_mask))
                    %     xline(treshold_low_conA,'--r')
                    %     %xline(treshold_high_conA,'--r')
                    %     title("conA - on the background")
                    %
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_dapi(image_mask))
                    %     xline(treshold_low_dapi,'--r')
                    %     xline(median_dapi,'--r')
                    %     xline(treshold_high_dapi,'--r')
                    %     title("dapi - on the cells")
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_dapi(~image_mask))
                    %     xline(treshold_low_dapi,'--r')
                    %     %xline(treshold_high_dapi,'--r')
                    %     title("dapi - on the background")
                    %
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_dapi(image_mask_dapi))
                    %     xline(treshold_low_dapi,'--r')
                    %     xline(median_dapi,'--r')
                    %     xline(treshold_high_dapi,'--r')
                    %     title("dapi2 - on the foreground  of the dapi chanel")
                    %
                    %
                    %     nexttile
                    %     histogram(image_MaxValues_dapi(~image_mask_dapi))
                    %     xline(treshold_low_dapi,'--r')
                    %     %xline(treshold_high_dapi,'--r')
                    %     title("dapi2 - on the background of the dapi chanel")
                    %
                    
                else %  else of if DETECT_TRESHOLDS_MODE
                    
                    %% read treshold values from the csv table
                    idx1=strcmpi(plate_treshold_table.YMP_folder_name,plateFolderName);
                    assert(sum(idx1)==1);
                    treshold_high_conA=plate_treshold_table{idx1,'treshold_high_conA'};
                    treshold_low_conA=plate_treshold_table{idx1,'treshold_low_conA'};
                    treshold_high_dapi=plate_treshold_table{idx1,'treshold_high_dapi'};
                    treshold_low_dapi=plate_treshold_table{idx1,'treshold_low_dapi'};
                    
                    
                    
                    
                    %% kiszamolom a gradiens kepekt ; a kovetkezo lepeben a fokuszkereseshez kelleni fognak
                    
                    for i_zStack=1:length(listOfImages_wp_conA)
                        image_conA=image_table.image_conA{i_zStack};
                        image_dapi=image_table.image_dapi{i_zStack};
                        
                        [dx, dy] = gradient(cast(image_conA,'double'));
                        image_table.gradient_Image_conA(i_zStack)= {(dx.^2) + (dy.^2)};
                        
                        [dx, dy] = gradient(cast(image_dapi,'double'));
                        image_table.gradient_Image_dapi(i_zStack)= {(dx.^2) + (dy.^2)};
                    end
                    
                    
                    %% megcsinalom a BW kepet amin az eloter es a hatter kulon van valasztva; zajszures: a sejtek belsejeben levo lyukakat feltoltom
                    
                    % fontos, h treshold alapjan legyen megcsinalva a BW kep,
                    % es ne a SD alapjan keszult maskbol,
                    % mert igy a vagasi hatarok teljesen feketek lesznek, nem
                    % fognak latszani
                    
                    image_BW= image_MaxValues_conA> treshold_low_conA | image_MaxValues_dapi> treshold_low_dapi;
                    %imshow(image_BW)
                    
                    % a nagyon pici hatter szinu connected compont-eket eldobom
                    cc=bwconncomp(~image_BW); % connected components
                    idx=cellfun(@length,cc.PixelIdxList)>100; % ahol tul keves pixel van, azt eldobom., mint zaj
                    pixelIdxList=cc.PixelIdxList(idx);
                    clearvars cc idx
                    
                    image_BW2=true(IMAGE_HEIGHT, IMAGE_WIDTH);
                    for i_cc =1:length(pixelIdxList)
                        image_BW2(pixelIdxList{i_cc}) = false;
                    end
                    
                    %imshow(image_BW2)
                    
                    % image_BW es az image_BW2 majdnem azonos, csak a masodikban keveseb a zaj
                    
                    %%  zajszures + sejtenkenti fokuszkereses ;  connected components-eket keresek, osszerekok egy-egy mixelt kepet
                    
                    cc=bwconncomp(image_BW2); % connected components
                    idx=cellfun(@length,cc.PixelIdxList)>5; % ahol tul keves pixel va, azt eldobom., mint zaj
                    pixelIdxList=cc.PixelIdxList(idx);
                    clearvars cc idx
                    
                    image_mixed_conA=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,'uint16');
                    image_mixed_dapi=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,'uint16');
                    
                    image_index_of_focus_conA=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,'uint8');
                    image_index_of_focus_dapi=zeros(IMAGE_HEIGHT, IMAGE_WIDTH,'uint8');
                    
                    for i_cc =1:length(pixelIdxList)
                        
                        img_1 =false(IMAGE_HEIGHT, IMAGE_WIDTH);
                        img_1(pixelIdxList{i_cc}) =true;
                        %imshow(img_1)    % mindenut fekete, csak egy connected component van rajta
                        
                        in_focus_score_conA=zeros(length(listOfImages_wp_conA),1);
                        in_focus_score_dapi=zeros(length(listOfImages_wp_conA),1);
                        for i_zStack=1:length(listOfImages_wp_conA)
                            
                            in_focus_score_conA(i_zStack)=mean( image_table.gradient_Image_conA{i_zStack}(img_1));  % ahol maximalis a score ott van fokuszban
                            in_focus_score_dapi(i_zStack)=mean( image_table.gradient_Image_dapi{i_zStack}(img_1));
                            
                        end
                        
                        [~,idx]=max(in_focus_score_conA);
                        image_mixed_conA(img_1)=image_table.image_conA{idx}(img_1);
                        image_index_of_focus_conA(img_1)=idx;
                        
                        [~,idx]=max(in_focus_score_dapi);
                        image_mixed_dapi(img_1)=image_table.image_dapi{idx}(img_1);
                        image_index_of_focus_dapi(img_1)=idx;
                        
                        
                        
                    end
                    
                    %imshow(uint8(uint16(image_index_of_focus_conA)*255/length(listOfImages_wp_conA)));
                    %imshow(uint8(uint16(image_index_of_focus_dapi)*255/length(listOfImages_wp_conA)));
                    
                    imwrite(uint8(uint16(image_index_of_focus_conA)*255/length(listOfImages_wp_conA)), ...
                        fullfile(outputFolder_debug1,   [wvp, '-index_of_focus_conA.png']));
                    
                    imwrite(uint8(uint16(image_index_of_focus_dapi)*255/length(listOfImages_wp_conA)), ...
                        fullfile(outputFolder_debug1,   [wvp, '-index_of_focus_dapi.png']));
                    
                    
                    %     imshow(imadjust(image_mixed_conA));
                    % imshow(imadjust(image_mixed_dapi));
                    
                    % a dapi chanelen lathato a sejtmagok koruli udvar,
                    % de azt a color scaleing el fogja tuntetni
                    
                    
                    %%  color scaleing
                    
                    
                    
                    image_mixed_colorScaled_conA = uint8( 255.0*  double(image_mixed_conA - treshold_low_conA) / double(treshold_high_conA-treshold_low_conA) );
                    image_mixed_colorScaled_dapi = uint8( 255.0*  double(image_mixed_dapi - treshold_low_dapi) / double(treshold_high_dapi-treshold_low_dapi) );
                    
                    
                    %imshow(image_mixed_colorScaled_conA);
                    %imshow(image_mixed_colorScaled_dapi);
                    %histogram(image_mixed_colorScaled_conA(image_mixed_colorScaled_conA>0));
                    %histogram(image_mixed_colorScaled_dapi(image_mixed_colorScaled_dapi>0));
                    
                    imwrite(image_mixed_colorScaled_conA,   fullfile(outputFolder_debug1,   [wvp, '-focused_colorScaled_conA.jpg']));
                    imwrite(image_mixed_colorScaled_dapi,   fullfile(outputFolder_debug1,   [wvp, '-focused_colorScaled_dapi.jpg']));
                    
                    %imwrite(image_mixed_colorScaled_dapi,   fullfile(outputFolder_debug1,   [wvp, '-focused_colorScaled_dapi-2.jpg']), 'Mode','lossless','BitDepth',16);
                    
                    
                    % csak a poen kedveert egymasra teszem a conA es a dapi chanelt
                    rgb_image=cat(3, image_mixed_colorScaled_dapi,image_mixed_colorScaled_conA,image_mixed_colorScaled_conA);
                    %     imshow(rgb_image)
                    imwrite(rgb_image,   fullfile(outputFolder_debug1,   [wvp, '-rgb.jpg']));
                    %% kepek felvagasa 4 reszre
                    
                    
                    
                    
                    
                    
                    
                    
                    for xi = 1:size(OUT_IMG_BOUNDARIES,1)
                        small_img_conA=image_mixed_colorScaled_conA(OUT_IMG_BOUNDARIES(xi,3):OUT_IMG_BOUNDARIES(xi,4),OUT_IMG_BOUNDARIES(xi,1):OUT_IMG_BOUNDARIES(xi,2)) ;
                        small_img_dapi=image_mixed_colorScaled_dapi(OUT_IMG_BOUNDARIES(xi,3):OUT_IMG_BOUNDARIES(xi,4),OUT_IMG_BOUNDARIES(xi,1):OUT_IMG_BOUNDARIES(xi,2)) ;
                        
                        imwrite(small_img_conA, fullfile(outputFolder1,  [wellName '-C' num2str(out_file_counter) '.jpg']));
                        imwrite(small_img_dapi, fullfile(outputFolder1,  [wellName '-D' num2str(out_file_counter) '.jpg']));
                        
                        fprintf(outputFileID2,"%s,%s,%s,%i\n",plateFolderName,wellName, wvp,out_file_counter);
                        
                        out_file_counter = out_file_counter + 1;
                    end
                    
                    
                    %%
                    
                end % end of if DETECT_TRESHOLDS_MODE
                
                
            end
        end
    end % for over wells
end

if DETECT_TRESHOLDS_MODE
    fclose(outputFileID);
else
    fclose(outputFileID2);
end
