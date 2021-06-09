clear; clc; clf; % clear everything
tic; % start a time counter

%% pre GEM info

% parameters and replicates
    b = 1; % birth rate
    d = 0.5; % death rate
    num_replicates = 50; % number of times to run GEM

% evolution and solution details
    h_2 = 1; % narrow-sense heritability
    cv = 0.3; % coefficient of variation of trait
    y0 = 5; % starting population size
    t_max = 6; % time span to run simulations    
    tspan = [0 t_max]; % start and end times

%% set up and run GEM 

[stand_times, R_data_out, x_data_out, x_var_data_out] = GEMv2_exponential_growth(b, d, cv, h_2, num_replicates, y0, t_max);

%% Solve non-evolutionary ODE for realized initial conditions

realized_initial_b = x_data_out(1,1); % get the average of the realized starting b's
realized_initial_b_var = x_var_data_out(1,1); % get the average of the realized variances of b

ode = @(t,y) EG_model(t,y,realized_initial_b,d); % compile function and call
    [t1,y1] = ode45(ode, tspan, y0); % return time and population density vectors

%% plotting
figure(1); % plot medians and ci's overtop individual lines
    subplot(3,1,1); box on;
        jbfill(stand_times,R_data_out(2,:),R_data_out(3,:),[0.5 0.5 0.5],'w',1,0.2); 

% X=[stand_times,fliplr(stand_times)];                %#create continuous x value array for plotting
% Y=[R_data_out(2,:),fliplr(R_data_out(3,:))];              %#create y values for out and then back
% fill(X,Y,[0.5 0.5 0.5]); alpha 0.2

        hold on;
        plot(stand_times,R_data_out(1,:),'-','Color','b','LineWidth',2);
        plot(t1,y1(:,1),'-k','LineWidth',2);
        xlim([0 t_max]);
        ylabel('Population size','FontSize',12);     
    subplot(3,1,2); box on;
        jbfill(stand_times,x_data_out(2,:),x_data_out(3,:),[0.5 0.5 0.5],'w',1,0.2); 
        hold on;
        plot(stand_times,x_data_out(1,:),'-','Color','b','LineWidth',2);
        plot([0 t_max],[realized_initial_b realized_initial_b],'--k');
        xlim([0 t_max]);
        ylabel('Mean birth rate','FontSize',12);
    subplot(3,1,3); box on;
        jbfill(stand_times,x_var_data_out(2,:),x_var_data_out(3,:),[0.5 0.5 0.5],'w',1,0.2); 
        hold on;
        plot(stand_times,x_var_data_out(1,:),'-','Color','b','LineWidth',2);
        plot([0 t_max],[realized_initial_b_var realized_initial_b_var],'--k');
        xlim([0 t_max]);
        ylabel('Variance in birth rate','FontSize',12);
        xlabel('Time','FontSize',12);

toc