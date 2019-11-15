import argparse
import json
import pandas as pd
import pyarrow.feather as feather
import sys


def main(args):
    data = load_file(args['file'][0])
    df = pd.DataFrame(data)
    df_cleaned = clean_data(df)

    # feather.write_feather(df_cleaned, 'CDs_and_Vinyl')
    df_cleaned.to_csv('CDs_and_Vinyl.csv', index=False)


def load_file(file):
    '''Loads json file that was passed in by user'''

    data = [json.loads(line) for line in open(file, 'r')]

    return data


def clean_data(dataframe):
    '''Takes pandas dataframe and cleans the data'''

    dataframe = dataframe.drop(['imUrl', 'related', 'salesRank'], axis=1)

    data_cleaned = dataframe['categories'].apply(pd.Series)\
        .merge(dataframe, right_index=True, left_index=True) \
        .drop(["categories"], axis=1) \
        .melt(id_vars=['asin', 'description', 'title', 'price', 'brand'],
              value_name="categories")\
        .drop("variable", axis=1) \
        .dropna(subset=['categories'])

    data_cleaned['categoriesstring'] =  \
        [','.join(map(str, l)) for l in data_cleaned['categories']]
    data_cleaned = data_cleaned.drop(columns=['categories'])

    return data_cleaned


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='clean data for visualizing')
    parser.add_argument('file', nargs=1, help='path to the data file')
    args = parser.parse_args()
    args = vars(args)

    main(args)
