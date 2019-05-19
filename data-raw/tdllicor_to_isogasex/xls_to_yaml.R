write_yaml(as.yaml(t(VARIABLES)), "isogasex_template4.yaml")

VAR2 <-
  "isogasex_template4_byhand.yaml" %>%
  read_yaml() %>%
  yaml.load()


