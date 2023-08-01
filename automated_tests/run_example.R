library(nistdecisiontree)

set.seed(123)

res = run_full_ndt(dataset = example_dataset,
                   exclude = rep(F,nrow(example_dataset)),
                   procedure = 'Recommended',
                   num_bootstrap = 1000,
                   seed = 123,
                   n_iter = 25000,
                   burn_in = 12500,
                   thin = 10)

summary_table(res)         # returns table of lab uncertainties
get_doe_plot(res)              # displays the DoE plot
get_doe_table(res)         # returns DoE Table
get_MCMC_diagnostics(res)  # returns MCMC Diagnostics Table
get_KCplot(res)            # displays KCRV and lab plot
