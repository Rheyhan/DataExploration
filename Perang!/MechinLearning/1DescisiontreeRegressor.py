#material: 
# -https://towardsdatascience.com/decision-tree-regressor-a-visual-guide-with-scikit-learn-2aa9e01f5d7f
# -https://www.geeksforgeeks.org/python-decision-tree-regression-using-sklearn/

#Decision Tree Regressor: 
#It’s used to solve regression problems. For example, prediction of how many people will die because of an opiate overdose.

#libraries, chers mate!
from sklearn.tree import DecisionTreeRegressor
import pandas as pd
from sklearn.tree import export_graphviz 
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from six import StringIO 
from IPython.display import Image  
import pydotplus

if "__main__" == __name__:
    
    def funnidecor(func):
        def decisiontree():
            X,y = func()    
            xtrain, xtest, ytrain, ytest=train_test_split(X, y, test_size=0.10)  #spliting Training and test, test 10% and training 90% split
            model = DecisionTreeRegressor(random_state=1, max_depth=3) #random_state: randomness || max_depth: depth of the tree
            model.fit(xtrain,ytrain)
            
            #model accuracy score.
            score=model.score(xtrain,ytrain)
            print(score)
            
            #predict and check accuract of predicted
            ypred = model.predict(xtest)
            mse = mean_squared_error(ytest, ypred)
            print("MSE: ", mse)
            print("RMSE: ", mse**(1/2.0)) 
            
            #export visualization of the tree map
            dot_data = StringIO()
            export_graphviz(model, out_file =dot_data, feature_names=['Rooms', 'Bathroom', 'Landsize', 'Lattitude', 'Longtitude'], rounded=True)
            graph = pydotplus.graph_from_dot_data(dot_data.getvalue())  
            graph.write_png('regressor.png')
            Image(graph.create_png())
            return None
        return decisiontree
    
    @funnidecor    
    def selectingdata():
        path="D:/Kuliah/Semester 4/AED/Perang!/MechinLearning/DF/melb_data.csv"
        df=pd.read_csv(path)
        df=df.dropna()
        #df.columns()     #check all avaialble col
        #df.head()
        selectedX=['Rooms', 'Bathroom', 'Landsize', 'Lattitude', 'Longtitude']
        X=scale(df[selectedX])
        #X.describe()     #kinda the same as R summary
        y=scale(df.Price)
        return X,y 
        
    selectingdata()
    
    
    