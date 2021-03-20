import time
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn import svm
from sklearn import datasets

# treat the first line of csv as table header.
raw_data = pd.read_csv("./train_binary.csv", header=0)
data = raw_data.values

# column 1 is the pos for labels data.
# [ 1 0 0 ]
# [ 1 0 0 ]
# [ 1 0 0 ]
# [ 1 0 0 ]
# The csv is too big.
# features = data[::, 1::]
# labels = data[::, 0]

iris = datasets.load_iris()
features = iris.data
labels = iris.target

# split data into training data and test data.
train_features, test_features, train_labels, test_labels = \
    train_test_split(
            features, labels, test_size=0.33, random_state=0)

start_time = time.time()
print("Starting training...")
# using SVM
classifier = svm.SVC()
classifier.fit(train_features, train_labels)
end_time = time.time()
print("training cost {} seconds".format(start_time - end_time))

print("Start predicting")
test_predict = classifier.predict(test_features)

score = accuracy_score(test_labels, test_predict)
print("The accuracy is {}".format(score))
