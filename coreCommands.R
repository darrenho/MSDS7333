pkgs = c(
  "dplyr", "gapminder", "ggplot2", "jsonlite", "Lahman", 
  "lubridate", "modelr", "nycflights13", "purrr", "readr", 
  "stringr", "tibble", "tidyr"
)
install.packages(pkgs)

require(nycflights13)
require(ggplot2)

require(dplyr) 
###
# Some dplyr notes:
#   - R namespace collisions: need to use e.g. stats::filter(), base::intersect()
#   - Key functions:
#       -Pick observations by their values: filter()
#       -Reorder the rows: arrange()
#       -Pick variables by their names: select()
#       -Create new variables with functions of existing variables: mutate()
#       -Collapse many values down to a single summary: summarise()
#   - All can be used in conjunction with group_by()
#   - All functions, "verbs", work similarly:
#       -The first argument is a data frame -> allows for "pipe" functionality
#       -The subsequent arguments describe what to do with the data frame. 
#        You can refer to columns in the data frame directly without using $.
#       -The result is a new data frame.
###

##
#filter
##
jan1 = filter(flights, month == 1, day == 1)
filter(flights, month == 11 | month == 12)
filter(flights, arr_delay <= 120, dep_delay <= 120)
#have to manually retain NAs
(tmp = tibble(x = c(1, NA, 3))); filter(tmp, is.na(x) | x > 1)

dim(filter(flights, dest=='HOU'| dest=='IAH'))

#can also use for leq and geq, coded in C++, works with SQL
#x <- rnorm(1e2)
#x[between(x, -1, 1)]

###
# arrange
###
tmp = tibble(x = c(1, NA, 3))
arrange(tmp, desc(x))

###
# select
###
out = arrange(flights, desc(dep_delay))
head(select(out,year:day,carrier,origin,dest))

#keeps all covariates that have time in name (not case sensitive)
select(flights, contains("TIME"))

###
# mutate
###
#make smaller
flights_sml = select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)

#transmute only retains new features
flights_sml_new = mutate(flights_sml,
                         gain = arr_delay - dep_delay,
                         speed = distance / air_time * 60)
head(arrange(flights_sml_new,desc(gain)))
#human readable times
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

###
# summarise: results in 1 row
###
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
#unless we use group_by
by_day = group_by(flights, year, month, day)
plt = ggplot(data=summarise(by_day, delay = mean(dep_delay, na.rm = TRUE)),
       mapping = aes(x=month,y=delay)) + geom_point() + 
       geom_smooth(se = FALSE)
plt + theme(axis.text.x = element_text(face="bold", color="#993333", 
                          size=14, angle=45))
plt + scale_x_continuous(name ="Month (Jan is 1 ...)", 
                         breaks=1:12, limits=c(.5,12.5))


###
# How to use "pipes"
#  x %>% f(y) turns into f(x, y)
#  x %>% f(y) %>% g(z) turns into g(f(x, y), z) ..
###

#not using pipes
by_dest = group_by(flights, dest)
delay   = summarise(by_dest,
                    count = n(),
                    dist = mean(distance, na.rm = TRUE),
                    delay = mean(arr_delay, na.rm = TRUE)
                    )
delay   = filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#using pipes
delays = flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

###
# Removing NAs (cancelled flights are recorded as NA)
###
not_cancelled = flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(avg_delay = mean(dep_delay))


not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  ) %>%
ggplot(mapping = aes(x = delay)) + 
  geom_histogram(binwidth = 5)+coord_cartesian(ylim = c(0, 50))

#which destination has largest variation in distance?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd)) %>%
ggplot(mapping = aes(x = distance_sd)) + 
  geom_histogram(binwidth = 1)+coord_cartesian(ylim = c(0, 50))


###
# Missing values
###
flights %>% 
  mutate(
    cancelled      = is.na(dep_time),
    sched_hour     = sched_dep_time %/% 100,
    sched_min      = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)