%% Parse CSV log files from GCS
% Requires JSONlab for parsing JSON: http://www.mathworks.com/matlabcentral/fileexchange/33381-jsonlab--a-toolbox-to-encode-decode-json-files
%

clear all;
close all;
clc;

filename = uigetfile('.csv');%'2017-03-22-141044-log.csv';
fid = fopen(filename); % open file
C = textscan(fid, '%s'); % scan file into cell array
% RelevantLine=C{1}{LineNum};

logtime=C{1}{1}; % timestamp

% iterate starting at line 2
for k=2:length(C{1})
    line_raw = C{1}{k};
    
    if ~isempty(strfind(line_raw,'num_sv')) % process only if we have the status packet
        m = strfind(line_raw,'{'); % find curly brackets
        m = m(1); % worry about only the first curly bracket
        time = line_raw(1:m-2); % subtract 2 to remove the brace and the collon
        line = line_raw(m:end); % separate data
        new_data = loadjson(line); % get a struct from json
        parsed_data{k-1} = new_data;
        parsed_data{k-1}.time = time;
    end
end

%% now traverse throught the data to get the relevant info
data = parse_data_struct(parsed_data);


%% finally show some plots
figure;
a1 = subplot(2,1,1);
hold on; grid on;
plot(data.timestamp, data.alt_est);
xlabel('Time[s]')
ylabel('Alt[m]')
legend('alt est')
title(filename);

a2 = subplot(2,1,2);
hold on; grid on;
plot(data.timestamp, data.rcinput,'b-.');
plot(data.timestamp, data.arming_mode,'r--');
plot(data.timestamp, data.thr_mode,'m-*');
plot(data.timestamp, data.px4io,'c-o');
xlabel('Time[s]')
ylabel('Status')
legend('rc input','arm mode', 'thr mode', 'px4io')
title(filename);

linkaxes([a1 a2],'x')

figure;
hold on; grid on;
plot(data.timestamp, data.battery);
xlabel('Time[s]')
ylabel('Voltage[V]')
legend('battery voltage')
title(filename);

figure;
hold on; grid on;
plot(data.timestamp, rad2deg(data.roll),'b-');
plot(data.timestamp, rad2deg(data.pitch),'r-');
plot(data.timestamp, rad2deg(data.yaw),'m-');
xlabel('Time[s]')
ylabel('Attitude[deg]')
legend('roll','pitch','yaw');
title(filename);
