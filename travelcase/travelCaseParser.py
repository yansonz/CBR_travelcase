from xlrd import open_workbook
from xlwt import Workbook
import csv

titles = ['Case', 'JourneyCode', 'HolidayType', 'Price', 'NumberOfPersons', 'Region', 'Transportation', 'Duration', 'Season', 'Accommodation', 'Hotel']
#titles = ['object', 'case', 'JourneyCode', 'HolidayType', 'Price', 'NumberOfPersons', 'Region', 'Transportation', 'Duration', 'Season', 'Accommodation', 'Hotel']
FIRST_ROW = 0
FIRST_COLUMN = 0
FIRST_SHEET = 0

data_length = len(titles) # 11

rb = open_workbook("TRAVEL.XLS")
wb = Workbook(encoding='utf-8')
newDataset = wb.add_sheet("newDataSet")

def stripStr(x):
    if(isinstance(x, unicode)):
        x = x.rstrip(".")   # HolidayType, Region, Transportation, Season, Accommodation
        x = x.rstrip(',')   # Hotel

    return x

def isEmpty(value):
    bEmpty = False
    if(value == ""):
        bEmpty = True
    else:
        bEmpty = False
    return bEmpty

# setting the title
index_col = FIRST_COLUMN
for title in titles:
    newDataset.write(FIRST_ROW, index_col, title)
    index_col = index_col + 1

first_sheet = rb.sheet_by_index(FIRST_SHEET)    # first worksheet
cells = first_sheet.col_slice(colx=2)   # the column which contains values
index_row = FIRST_ROW + 1   # referring to the row of cell where the value will be saved
index_col = FIRST_COLUMN    # referring to the column of cell where the value will be saved

for cell in cells:
    if(index_col > data_length - 1):
        index_row = index_row + 1
        index_col = 0

    if isEmpty(cell.value) and index_col == 0:
        continue
    else:
        newDataset.write(index_row, index_col, stripStr(cell.value))
        index_col = index_col + 1

wb.save("newDataSet.xls")

#####
workbook = open_workbook("newDataSet.xls")
worksheet = workbook.sheet_by_name("newDataSet")
csvfile = open("newDataSet.csv", 'wb')
wr = csv.writer(csvfile, quoting=csv.QUOTE_ALL)

for rownum in xrange(worksheet.nrows):
 wr.writerow(worksheet.row_values(rownum))

csvfile.close()
