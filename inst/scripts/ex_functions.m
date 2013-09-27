%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example file for the R package RcppOctave
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [a] = fun1()
	a = rand(1,4);
end

function [a,b,c] = fun2()
	a = rand(1,4);
	b = rand(2,3);
	c = "some text";
end

function fun_noargout(x) 
	% no effect outside the function
	y = 1;
	printf("%% Printed from Octave: x="), disp(x);
end

function [s] = fun_varargin(varargin)
  if (nargin==0)
	s = 0;
  else
	s = varargin{1} + varargin{2} + varargin{3};
  endif
end

function [u, s, v] = fun_varargout()

	if (nargout == 1) u = 1; 
	elseif (nargout == 3)
		u = 10; s = 20; v = 30; 
	else usage("Expecting 1 or 3 output variables.");
	endif; 
end

