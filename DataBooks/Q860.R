


Q860_tibbles = map(map(c("Q860A", "Q860B", "Q860C", "Q860D"),sym), function(x){ x=enquo(x)
    abv%>%group_by(country)%>%filter(!!x%in%c(0:10))%>%summarise(x= mean(!!x))%>%`colnames<-`(c("country", x))  })%>%
  set_names(c("Q860A", "Q860B", "Q860C", "Q860D"))
Q860_tibbles_joined = map(Q860_tibbles[-1], function(x) inner_join(x,Q860_tibbles$Q860A ))

fully_joined = full_join(full_join(Q860_tibbles_joined[[1]], Q860_tibbles_joined[[2]]), Q860_tibbles_joined[[3]])%>%
  `colnames<-`(c("country", "Q860B", "Q860A", "Q860C", "Q860D"))

estimates  = fully_joined%>%
  mutate_at(c("Q860B", "Q860C","Q860D"),  function(x)x-fully_joined$Q860A)%>%
  recode_country()

clean_estimates = map(map(c("Q860B", "Q860C", "Q860D"),sym), function(x){ x=enquo(x)
    estimates%>%
      filter(!!x>0)%>%
      select(Country,!!x)})%>%
  set_names(c("Q860B", "Q860C", "Q860D"))

Q860_plots  = map(c("Q860B", "Q860C", "Q860D"), ~plotterizer(dataframe = clean_estimates[[.x]], x=.x))







