import os.path
import re
import os

def getDocumentList():
    documentList = [] 
    folderPath = os.getcwd()
    for root, dirs, files in os.walk(folderPath):
        for file in files:
            if file.endswith('.txt'):
                documentList.append(os.path.join(root,file))
    return documentList

def getValues(spectrum, valueName):
    valueName = [item for item in spectrum if item.startswith(valueName)]
    values = re.split("\n", spectrum[spectrum.index(valueName[0]) + 1])
    values = [x for x in values if x]
    return values, valueName[0]

def listToString(value):
    return ''.join(value[0])

def formColNames(mca, dateMEA, measTIM, doseRate, temperature, dataAmount):
    head = [mca, dateMEA, measTIM, doseRate, temperature, "CPS"]
    for i in range(1, dataAmount + 1):
        head.append("POINT_" + str(i))
    for i in range(1, dataAmount + 1):
        head.append("CPS_POINT_" + str(i))
    return head

def formColValues(mca, dateMEA, measTIM, doseRate, temperature, cps, data):
    rest = [mca, dateMEA, measTIM, doseRate, temperature, cps]
    for value in data:
        rest.append(value)
    for value in data:
        cps = float(value) / float(measTIM)
        rest.append(cps)
    return rest

def isFileExist():
    return os.path.exists(outputName)

def FormDataSet(filePath):
    with open(filePath, 'r', encoding='utf-16-le') as file:
        lines = []
        for line in file:
            if "$ENER_FIT:" in line:
                break
            lines.append(line.strip())

    doc = '\n'.join(lines)
    
    dataSet = []

    for column, value in re.findall(r'\$(\w+):([\s\S]*?(?=\$|$))', doc):
        dataSet.append(column)
        dataSet.append(value)
        
    #print(dataSet)
    
    
    mca_value, mca_name = getValues(dataSet, "MCA_")
    mca_value = re.findall('\d+', ''.join(mca_value))

    dateMEA_value, dateMEA_name = getValues(dataSet, "DATE_MEA")

    measTIM_value, measTIM_name = getValues(dataSet, "MEAS_TIM")
    measTIM_value = re.split(" ", measTIM_value[0])
    measTIM_value = ''.join(measTIM_value[0])

    doseRate_value, doseRate_name = getValues(dataSet, "DOSE_RATE")

    temperature_value, temperature_name = getValues(dataSet, "TEMPERATURE")

    data_value, data_name = getValues(dataSet, "DATA")
    data_value = data_value[1:len(data_value) - 1]
    
    data_value = [float(i) for i in data_value]
    
    cps = sum(data_value)/float(measTIM_value)
    
    mca_value = listToString(mca_value)
    dateMEA_value = listToString(dateMEA_value)
    doseRate_value = listToString(doseRate_value)
    temperature_value = listToString(temperature_value)

    head = formColNames(mca_name, dateMEA_name, measTIM_name, doseRate_name, temperature_name, len(data_value))
    
    rest = formColValues(mca_value, dateMEA_value, measTIM_value, doseRate_value, temperature_value, cps, data_value)
    
    if isFileExist() == False:
        with open(outputName, "w") as file:
                file.write('\t'.join(str(item) for item in head) + "\n")
            
    with open(outputName, "a") as file:
            file.write('\t'.join(str(item) for item in rest) + "\n")


outputName = "dataSet.txt"

documentList = getDocumentList()

print(documentList)

for documentPath in documentList:
    FormDataSet(documentPath)