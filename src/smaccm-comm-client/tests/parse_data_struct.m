function parsed_data = parse_data_struct(data)
% parsing this struct
% 
%              num_sv: 0
%             rcinput: 'Negative'
%                 yaw: 1.2866
%         arming_mode: 'Safe'
%                roll: 0.0178
%     battery_voltage: 11.6807
%             heading: 0
%       control_modes: [1×1 struct]
%                 lat: 0
%       gyro_progress: 0
%             alt_est: 0.1379
%                 fix: 'FixNone'
%            sens_cal: 'Neutral'
%               px4io: 'Negative'
%               telem: 'Neutral'
%                 lon: 0
%             vground: 0
%               valid: 1
%               pitch: 0.0191
%        mag_progress: 0
%                 alt: 0
%                time: '6.407416'

l = length(data);    
time = nan(1,l);
rcinput = nan(1,l);
arming_mode = nan(1,l);
roll  = nan(1,l);
battery = nan(1,l);
yaw = nan(1,l);
thr_mode = nan(1,l);
alt_est = nan(1,l);
px4io = nan(1,l);
pitch = nan(1,l);

    for k=1:l
        
        if ~isempty(data{k})
        
            % timestamp
            time(k) = str2double(data{k}.time);

            % rc input
            if strfind(data{k}.rcinput,'Negative')
                rcinput(k) = -1;
            else if strfind(data{k}.rcinput,'Neutral')
                    rcinput(k) = 0;
                else
                    rcinput(k) = 1;
                end
            end

            % arming mode
            if strfind(data{k}.arming_mode,'Safe')
                arming_mode(k) = -1;
            else
                arming_mode(k) = 1;
            end

            % yaw
            yaw(k) = data{k}.yaw;
            % pitch
            pitch(k) = data{k}.pitch;
            % roll
            roll(k) = data{k}.roll;

            % battery
            battery(k) = data{k}.battery_voltage;

            % alt est
            alt_est(k) = data{k}.alt_est;

            % px4io
            if strfind(data{k}.px4io,'Negative')
                px4io(k) = -1;
            else if strfind(data{k}.px4io,'Neutral')
                    px4io(k) = 0;
                else
                    px4io(k) = 1;
                end
            end

            % control_modes
            if strfind(data{k}.control_modes.thr_mode,'DirectUi')
                thr_mode(k) = -1;
            else if strfind(data{k}.control_modes.thr_mode,'AltUi')
                    thr_mode(k) = 0;
                else
                    thr_mode(k) = 1;
                end
            end
        
        end
        
    end
    
    % package the struct
    parsed_data.timestamp = time;
    parsed_data.rcinput = rcinput;
    parsed_data.arming_mode = arming_mode;
    parsed_data.yaw = yaw;
    parsed_data.pitch = pitch;
    parsed_data.roll = roll;
    parsed_data.battery = battery;
    parsed_data.thr_mode = thr_mode;
    parsed_data.alt_est = alt_est;
    parsed_data.px4io = px4io;
end
