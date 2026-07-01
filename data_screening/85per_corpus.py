import pandas as pd
df = pd.read_csv('./output/corpus_density_scored.csv')
threshold = df['density'].quantile(0.85)
filtered = df[df['density'] >= threshold]
filtered.to_csv('./output/corpus_85th_percentile.csv', index=False)
print(len(filtered))