function [stand_times, R_data_out, x_data_out, x_var_data_out] = GEMv2_exponential_growth(b, d, cv, h_2, num_replicates, y0, t_max)

% this is a GEM for exonential growth
% the evolving trait is intrinsic rate of population growth
% call this function to run it
% NEEDS THESE FUNCTIONS: 'jbfill', 'pick_individuals'

stand_times = 0:0.5:t_max; % standardized time steps for storing time series
num_time_steps = length(stand_times); % calculate the number of standardized time steps

R_stand = nan(num_replicates,num_time_steps); % preallocate matrix for standardized population size
x_stand = nan(num_replicates,num_time_steps); % preallocate matrix for trait
x_var_stand = nan(num_replicates,num_time_steps); % preallocate matrix for trait variance

for i = 1:num_replicates % start Gillespie algorithm
    i % shows where you are in the run in the Command Window
    % preallocate for whole time series
        R = zeros(1,1e7); % 1e7 is just a large number to ensure the vector is long enough
        x_mean = nan(1e7,1); % preallocate for mean trait
        x_var = nan(1e7,1); % preallocate for trait variance
        t = nan(1,1e7); % preallocate for time steps
    % create initial distribution for parameter
        rng('shuffle'); % change random number seed
    % specify initial conditions
        t(1) = 0; % initial time
        R(1) = y0(1); % initial population size
    % pull initial distribution of trait
        x_dist_init = pick_individuals(b,cv*b,R(1));
        x_dist = x_dist_init; % reset trait distribution at the start of each simulation
        x_mean(1) = mean(x_dist); % initial mean trait
        x_var(1) = var(x_dist); % initial variance in trait
        
    count = 1; % start counter to index steps while inside loop
    while t(count) < t_max
        if sum(x_dist(:)~=0) > 0 % as long as population size is > 0, pick another individual
            whosnext = randi(size(x_dist,1),1); % randomly choose individual from the vector
            x_next = x_dist(whosnext); % pick the trait for that individual
        end

        % set up rates of each possible event
        % birth
            b_R = x_next*R(count);
        % death
            d_R = d*R(count);

    % sum the events to make wheel of fortune
        CS_vector = cumsum([b_R d_R]); % calculate cumulative sum of terms
        Slice_widths = CS_vector./CS_vector(end); % probabilities are each term divided by total sum
        LI = rand < Slice_widths; % randomly pick a number (rand) and determine if it is less than slices
        Event_index = find(LI,1,'first'); % pick the first slice rand is less than

    % now choose actual events
        if Event_index == 1 % choose birth of prey            
            x_parent = (1-h_2)*mean(x_dist_init) + h_2*x_next; % offspring trait distribution mean
            off_std = sqrt(1-h_2^2)*((1-h_2)*std(x_dist_init)+h_2*std(x_dist)); % offspring trait distribution std
            x_dist(size(x_dist,1)+1) = pick_individuals(x_parent,off_std,1); % return trait
            
        elseif Event_index == 2 % choose death
            x_dist = x_dist([1:whosnext-1,whosnext+1:end]); % reduce dist by lost individual
        end
        
            R(count+1) = size(x_dist,1); % count population size
            x_mean(count+1) = mean(x_dist); % calculate new mean trait
            x_var(count+1) = var(x_dist); % calculate new variance trait

        t(count+1) = t(count) + exp(-1/CS_vector(end))/CS_vector(end); % updating time
        count = count+1; % advance time steps
    end

    % find standardized times and corresponding densities (need for ci's)    
        tmp = abs(t(~isnan(t))-stand_times');
        minima = min(tmp,[],2);
        idx = tmp==minima;
        touse = sum(idx,1);

        R_stand(i,1:length(R(touse==1))) = R(touse==1); % resource abundance at standard time
        x_stand(i,1:length(R(touse==1))) = x_mean(touse==1); % trait values at standard time
        x_var_stand(i,1:length(R(touse==1))) = x_var(touse==1); % trait values at standard time

end

% calculate ci's for time series
    upper_ci_level = 75; % choose ci levels
    lower_ci_level = 25; % choose ci levels
    % abundance
        test(:,:) = R_stand(:,:);
        ci_R_up = prctile(test,lower_ci_level);
        ci_R_down = prctile(test,upper_ci_level);
        median_R = prctile(test,50);
        R_data_out = [median_R; ci_R_up; ci_R_down];
    % trait
        test(:,:) = x_stand(:,:);
        ci_x_up = prctile(test,lower_ci_level);
        ci_x_down = prctile(test,upper_ci_level);
        test(test == 0) = NaN;
        median_x = prctile(test,50);
        x_data_out = [median_x; ci_x_up; ci_x_down];
    % variance in trait
        test(:,:) = x_var_stand(:,:);
        ci_x_var_up = prctile(test,lower_ci_level);
        ci_x_var_down = prctile(test,upper_ci_level);
        median_x_var = prctile(test,50);
        x_var_data_out = [median_x_var; ci_x_var_up; ci_x_var_down];