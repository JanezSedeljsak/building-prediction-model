import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.read_csv("ucna.txt")
#sns.pairplot(data=df, diag_kind='kde')
#plt.show()

sns.heatmap(df.corr(), cmap='Blues', annot=True)
plt.show()
