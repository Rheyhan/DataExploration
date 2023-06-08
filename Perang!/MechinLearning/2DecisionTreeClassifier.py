#materials
    #Decision Tree Classifier: 
    # It's used to solve classification problems. For example, they are predicting if a person will have their loan approved.
#https://www.datacamp.com/tutorial/decision-tree-classification-python


#libraries
from sklearn.tree import DecisionTreeClassifier
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.datasets import load_iris
from sklearn.metrics import accuracy_score
from sklearn.tree import export_graphviz 
from six import StringIO 
from IPython.display import Image  
import pydotplus

if __name__ == "__main__":
    def decisiontreemaking(func):
        def inner():
            df=func()
            X= df.drop(["Species"], axis=1)
            y= df.Species
            X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 69420)   #test: 0.2 || train: 0.8
            model=DecisionTreeClassifier(random_state=911)
            model.fit(X_train, y_train)
            predict= model.predict(X_test)
            print(f'accuracy: {accuracy_score(predict, y_test)}')
            return model, df
        return inner
    
    def visualize(func):
        def inner():
            model, df = func()      
            colnames=(df.columns).drop(["Species"])
            dot_data = StringIO()
            export_graphviz(model, out_file =dot_data, feature_names=colnames, class_names=(load_iris()).target_names, rounded=True)
            graph = pydotplus.graph_from_dot_data(dot_data.getvalue())  
            graph.write_png('learn.png')
            Image(graph.create_png())   
            return None
        return inner
    
    @visualize
    @decisiontreemaking
    def loaddf():
        iris=(load_iris()) #Menggunakan data iris
        df=pd.DataFrame(iris.data, columns = iris.feature_names)
        df["Species"]=iris.target
        df.dropna()
        return df
    
    loaddf() 