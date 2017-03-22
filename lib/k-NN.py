import numpy as np
import numpy.linalg as la
import scipy.special as scs
import matplotlib.pyplot as plt

def load_data():
    x_test = np.genfromtxt('/Users/ptfairy/Desktop/training_data/X_test.csv', delimiter=',')
    y_test = np.genfromtxt('/Users/ptfairy/Desktop/training_data/y_test.csv', delimiter=',')
    x_train = np.genfromtxt('/Users/ptfairy/Desktop/training_data/X_train.csv', delimiter=',')
    y_train = np.genfromtxt('/Users/ptfairy/Desktop/training_data/y_train.csv', delimiter=',')

def kNN_classification(x_train, y_train, x_test, y_test):
    # calculate the distance for each testing nodes:
    total_acurracy = []
    for k in range(1, 21):
        y_pred = []
        for x in x_test:
            dist = np.mean(np.abs(x_train - x), axis=1)

            idx = np.argpartition(dist, k)
            pred = 0
            if np.mean(y_train[idx[:k]]) > 0.5:
                pred = 1
            y_pred.append(pred)

        # compare the prediction and the original result
        y_pred = np.asarray(y_pred)

        error = np.mean(np.abs(y_pred - y_test))
        print("error rate at k=", k, "is", error)
        total_acurracy.append(1 - error)

    total_acurracy = np.asarray(total_acurracy) * 100
    print(total_acurracy)
    x_axis = np.arange(1, 21)
    plt.xlabel("$K$")
    plt.ylabel("Classification Accuracy (%)")
    plt.title("$k-NN$ Classification")
    plt.plot(x_axis, total_acurracy, 'b+-')
    plt.grid(True)
    plt.show()

    f __name__ == '__main__':
    # load data
    x_test, x_train, y_test, y_train = load_data()
    
    kNN_classification(x_train, y_train, x_test, y_test)
