from docx import Document
import pandas as pd
import docx2txt
import os.path
import re
import os

#functions

def getDocumentList():
    documentList = [] 
    folderPath = os.getcwd()
    for root, dirs, files in os.walk(folderPath):
        for file in files:
            if file.endswith('.docx'):
                documentList.append(os.path.join(root,file))
    return documentList

def isFileExist():
    return os.path.exists(outputFileName)


def saveValue(data):
    with open(outputFileName, "a") as file:
        file.write("\t".join(str(item) for item in data) + "\n")

def saveHeader(data):
    with open(outputFileName, "w") as file:
        file.write("ID\t"+"FolderName\t"+"\t".join(str(item) for item in data) + "\n")

def clearFreeSpace(list):
    for i in range(len(list)):
        list[i] = list[i].strip()

def formTableList(options, nuclides):
    list = []
    for i in nuclides:
        for j in options:
            list.append(i +"_"+ j)
    return list

def getTableSize(fileName, valueList):
    document = Document(fileName) 
    tables = document.tables

    df_tables = []
    for table in tables:
        df = [['' for i in range(len(table.columns))] for j in range(len(table.rows))]
        for i, row in enumerate(table.rows):
            for j, cell in enumerate(row.cells):
                if cell.text:
                    df[i] = cell.text
        df_tables.append(pd.DataFrame(df))

    col = (len(df_tables[0][0]))
    row = (len(valueList)+1)//col
    return col, row

def exportTable(fileName, text):
    valueList = text[text.index("RESULTS")+1:]
    
    col, row = getTableSize(fileName, valueList)

    options = valueList[:col-1]

    del valueList[:col-1]

    nuclides = valueList[::row]

    del valueList[::row]

    headList = formTableList(options,nuclides)
    return valueList, headList

def formSpectrumDateList(text):
    dataTime = ['date', 'time']
    timeHeadList = []

    headList = [text[text.index("Experimental spectrum")] , text[text.index("Background spectrum")]]

    for head in headList:
        for item in dataTime:
            timeHeadList.append(head +", "+ item)

    timeValueList = re.findall(r"\d{8}|\d{6}|\d{4}_\d{2}_\d{2}", text[text.index("Experimental spectrum")+1]) + re.findall(r"\d{8}|\d{6}|\d{4}_\d{2}_\d{2}", text[text.index("Background spectrum")+1])

    timeValueList = [re.compile(r"_").sub("", m) for m in timeValueList]

    if len(timeHeadList) != len(timeValueList):
        inequality = len(timeHeadList) - len(timeValueList)
        for i in range(inequality):
            timeValueList.append("")

    return timeHeadList, timeValueList

def formHeadList(headList, headListTable, positionList, timeHeadList, text):
    headList = text[:text.index("Geometry")]
    headList = headList[::2] + positionList[::2] + text[text.index("Position")::2] + headListTable

    for i in range(2):
        headList.insert(headList.index("Experimental spectrum") + i + 1, timeHeadList[i])
    
    for i in range(2):
        headList.insert(headList.index("Background spectrum") + i + 1, timeHeadList[i+2])
        
    return headList

def formPositionList(text):
    positionList = text[text.index("Geometry"):text.index("Position")]
    positionList = [x for xs in positionList for x in xs.split(',')]

    positionListBuff = positionList[2:4]
    positionListBuff = ','.join(positionListBuff)

    positionList = positionList[:2] + [positionListBuff] + positionList[4:]

    clearFreeSpace(positionList)

    return(positionList)

def formToTxt(fileName, id):
    text = docx2txt.process(fileName)
    text = "".join([text for text in text.strip().splitlines(True) if text.strip()])
    text = re.split(regex_pattern, text)

    valueListTable, headListTable = exportTable(fileName, text)

    timeHeadList, timeValueList = formSpectrumDateList(text) 

    text = text[text.index("Detection device SN"):text.index("RESULTS")]
    headList = text[:text.index("Geometry")]
    
    positionList = formPositionList(text)

    valueList = headList[1::2] + positionList[1::2] + text[text.index("Position") + 1::2] + valueListTable

    headList = formHeadList(headList, headListTable, positionList, timeHeadList, text)

    for i in range(2):
        valueList.insert(headList.index("Experimental spectrum") + i + 1, timeValueList[i])
    
    for i in range(2):
        valueList.insert(headList.index("Background spectrum") + i + 1, timeValueList[i+2])

    pattern = re.compile(r"± |-")
    valueList = [pattern.sub("", match) for match in valueList]

    if(os.path.dirname(fileName) != os.path.dirname(documentList[documentList.index(fileName) - 1])):
        id += 1
        valueList.insert(0, str(id))
        valueList.insert(1, os.path.basename(os.path.dirname(fileName)))
    else:
        valueList.insert(0, str(id))
        valueList.insert(1, os.path.basename(os.path.dirname(fileName)))

    clearFreeSpace(valueList)

    if os.path.getsize(fileName) == 0 or isFileExist() == False:
        saveHeader(headList)
        fileExists = True
    
    saveValue(valueList)
    return (id)

   
#vars  
#, "± "
id = 0

delimiters = ":", ": ", "\n\n\n","\n\n", "\n"

regex_pattern = '|'.join(map(re.escape, delimiters))

outputFileName = "output.txt"

documentList = getDocumentList()

for documentPath in documentList:
    id = formToTxt(documentPath, id)