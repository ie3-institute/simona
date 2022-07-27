function [Setpoints] = dcopf(Fall)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

define_constants;                          
mpc = loadcase(Fall);
results = rundcopf(mpc);
        
active_power = results.gen(:, PG);
gen_number = 1:length(active_power);

Setpoints = [gen_number', active_power];

end

