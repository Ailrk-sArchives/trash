# load iris dataset.
from sklearn.datasets import load_iris
iris = load_iris()

# feature and label
X = iris.data
y = iris.target

# seperate data into training set and test set.
from sklearn.model_selection import train_test_split
# random_state = 200 is a magic number for this model. DNW
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=200)


# trainning the model on trainning set
from sklearn.naive_bayes import GaussianNB
gnb = GaussianNB()
gnb.fit(X_train, y_train)

# making prediction on the test set
y_pred = gnb.predict(X_test)

# compare actual response value with predicted value
from sklearn import metrics
print("gnb Accuracy {}".format(metrics.accuracy_score(y_test, y_pred) * 100))
