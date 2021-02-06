import numpy as np
from sklearn import datasets

# load data
iris = datasets.load_iris()
iris_X = iris.data
iris_y = iris.target
np.unique(iris_y)

###################################
# K-Nearest neighbors classifier
###################################

# split iris into train and test data. use random permuation to
# split the data randomly.
np.random.seed(0)
indices = np.random.permutation(len(iris_X))
iris_X_train = iris_X[indices[:-10]]
iris_y_train = iris_y[indices[:-10]]

iris_X_test = iris_X[indices[-10:]]
iris_y_test = iris_y[indices[-10:]]

# create and fit a nearest neighbor classifier.
from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(
            n_neighbors=4,
            weights='distance',
            algorithm='brute'
        )  # classifier or regressor == estimator
knn.fit(iris_X_train, iris_y_train)

iris_y_pred = knn.predict(iris_X_test)

# evaluate the prediction with metrics
from sklearn import metrics
print(
        "KNN accuracy: {}".format(
            metrics.accuracy_score(iris_y_test,
                iris_y_pred) * 100)
     )

# the curse of dimensionality for KNN.

print()
###################################
# Linear Regression
###################################

# to minizie the sum of squared resoduals
from sklearn import linear_model
regr = linear_model.LinearRegression(normalize=True)
regr.fit(iris_X_train, iris_y_train)

print("LinearRegression coef {}".format(regr.coef_))

# The mean square error
np.mean((regr.predict(iris_X_test) - iris_y_test) ** 2)
print(
        "Linear regression accuracy: {}"
        .format(regr.score(iris_X_test, iris_y_test) * 100)
      )
