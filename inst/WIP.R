library(deepgs4)


sheet <- request_ss_get("1n58ggWFvbH_mnr126HmqlECmACGoOHdMZgcQAX_pOJ0")

copy_sheet <- request_ss_sheet_copy(
  "1n58ggWFvbH_mnr126HmqlECmACGoOHdMZgcQAX_pOJ0",
  0,
  "1ouitI0wEpeD3RAcfkSWsbgK-y4aqAVgWNfXsorUuBwM"
)
