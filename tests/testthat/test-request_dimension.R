# deepgs_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
#             cache = F)
# googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
#                         cache = F)
#
# spreadsheet <- send_create_req(
#   spreadsheet = Spreadsheet(
#     sheets = Sheet(
#       properties = SheetProperties(
#         sheetId = 0, title = "Test_Sheet",
#         gridProperties = GridProperties(
#           10, 10
#         )
#       )
#     )
#   )
# )
