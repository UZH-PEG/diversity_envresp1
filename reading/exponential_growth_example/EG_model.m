function dydt = EG_model(~,y,b,d)
dydt = zeros(size(y));

% variables
R = y(1);

dydt(1) = b*R - d*R;
