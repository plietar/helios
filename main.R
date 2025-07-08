# withr::with_envvar(list(PKG_LIBS = "-lprofiler"), {
#   cpp11::cpp_source(code = '
#     #include <cpp11.hpp>
#     #include <gperftools/profiler.h>
#     
#   [[cpp11::register]]
# void profiler_start(const char *path) {
#   printf("Starting profiler at %s\\n", path);
#   ProfilerStart(path);
# }
#     
# [[cpp11::register]]
# void profiler_stop() {
#   ProfilerStop();
# }
#   ')
# })

library(parallel)
library(dplyr)
library(ggplot2)


devtools::load_all()
do <- function(system, profile = NULL, pop = 50000, timesteps = 100, N = 10) {
    params <- helios::get_parameters(
      list(human_population = pop,
           number_initial_S = 0.8 * pop,
           number_initial_E = 0.1 * pop,
           number_initial_I = 0.08 * pop,
           number_initial_R = 0.02 * pop,
           simulation_time = timesteps,
           mob = system))

    description <- if (is.null(system)) "null" else system

    result <- local({
      if (!is.null(profile)) {
        profiler_start(profile)
        withr::defer(profiler_stop())
      }
      lapply(seq_len(N), function(i) {
        before <- Sys.time()
        data <- helios::run_simulation(params)
        after <- Sys.time()
        t <- as.double(after - before, "secs")
        cli::cli_alert_info("{description} {t}")
        list(t = t, data = data)
      })
    })

    times <- tibble(iteration = seq_along(result), time = purrr::map_dbl(result, "t"), mob = description)
    data <- purrr::list_rbind(purrr::imap(result, function(x, i) {
       dplyr::mutate(x$data, iteration = i, mob = description)
    }))
  
    list(times = times, data = data)
}

raw_results <- list(
  do(pop = 1000000, timesteps = 1000, N=1, system = "device"),
  do(pop = 1000000, timesteps = 1000, N=1, system = "host"))
  # do(pop = 10000, system = NULL))
# 
# results <- raw_results %>%
#   purrr::list_transpose() %>%
#   purrr::map(bind_rows)

# do(pop = 1000, timesteps = 10000, N=1, system = "device", profile="out.prof")

#   result <- local({
#     if (!is.null(profile)) {
#     }
#   })
# }
# 
# data <- results$data %>%
#   tidyr::pivot_longer(
#     ends_with("_count"),
#     names_pattern = "(.*)_count",
#     names_to = "compartment",
#     values_to = "value") %>%
#   group_by(timestep, mob, compartment) %>%
#   summarise(across(everything(), c(min=min, max=max)))
# 
# ggplot(data, aes(x=timestep, fill=mob)) +
#   geom_ribbon(aes(ymin=value_min, ymax=value_max), alpha = 0.5) +
#   facet_wrap(vars(compartment))
# ggsave("data.pdf")
# 
# ggplot(results$times, aes(time, mob, fill=mob)) +
#   geom_violin() +
#   geom_jitter(height = 0.1, width = 0)
# ggsave("times.pdf")
