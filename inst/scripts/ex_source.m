% Example m-file to illustrate the usage of the function o_source
%
% This file defines 3 dummy variables ('a','b' and 'c') 
% and a dummy function 'abc', that adds up its three arguments.
% 

a = 1;
b = 2;
c = 3;

function [res] = abc(x, y, z)
	res = x + y + z; 
end

