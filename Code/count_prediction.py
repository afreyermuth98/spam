import pyreadr

result = pyreadr.read_r('predictions_09012021_963Acc.rda')

test = result['rf_predict_data_test']

sum0 = 0
sum1 = 0
for i in df.iterrows():
    right = i[1]
    values = right.values
    value = float(values[0])
    if (value == 0):
        sum0 += 1
    if (value == 1):
        sum1 += 1

print("Numbers of spamms : ", sum1)
print("Numbers of hams : ", sum0)
