'''
Created on Oct 16, 2017

@author: alex
'''

import sys

import pandas as pd
import numpy as np


def read_dataframe_from_nswd_file(nswd_file):
    AXLE_COUNT = "N"
    columns = ["timestamp", "offset", "site id", "stage trace",
               "warning flags", "lane", "v", AXLE_COUNT, "subclass id", "axle groups", "gross weight"]

    axleCountPos = columns.index(AXLE_COUNT)
    NMax = 0
    MMax = 0
    with open(nswd_file, "r") as f:

        # Create a new DataFrame with mandatory columns
        df = pd.DataFrame()

        totalLineCount = 0
        for line in f:
            totalLineCount += 1

        for column in columns:
            df[column] = np.nan

        f.seek(0)
        lineCount = 0
        print("Total line count = " + str(totalLineCount))

        for line in f:
            fields = line.split()
            if len(fields) < 2:
                continue

            # columns creation if needed
            N = int(fields[axleCountPos])
            if N > NMax:
                for a in range(NMax, N):
                    df["load_{}".format(a + 1)] = np.nan
                    if a > 0:
                        df["d_{}".format(a)] = np.nan
                NMax = N

            MIndex = 15 + 2 * N - 1
            if MIndex <= len(fields) - 1:
                M = int(fields[MIndex])
                if M > MMax:
                    for a in range(M - MMax):
                        sumIndex = MMax + a + 1
                        df["sum_strip_{}".format(sumIndex)] = np.nan
                    MMax = M
            lineCount += 1
            if lineCount % 1000 == 0:
                print("Pass 1 : {}/{}".format(lineCount, totalLineCount))

        data = {}
        for columnName in df.columns.values:
            data[columnName] = []

        data["_long"] = []

        f.seek(0)
        lineCount = 0
        for line in f:
            fields = line.split()
            if len(fields) < 2:
                continue

            N = int(fields[axleCountPos])

            # data filling
            for i, name in enumerate(columns):
                data[name].append(fields[i])

            for i in range(N):
                data["load_{}".format(i + 1)].append(fields[11 + i])

            for i in range(N, NMax):
                data["load_{}".format(i + 1)].append(np.nan)

            data["_long"].append(fields[11 + N])

            for i in range(1, N):
                data["d_{}".format(i
                     )].append(fields[11 + N + i])

            for i in range(N, NMax):
                data["d_{}".format(i)].append(np.nan)

            lineCount += 1
            if lineCount % 1000 == 0:
                print("Pass 2 : {}/{}".format(lineCount, totalLineCount))

    for columnName in data:
        serie = pd.Series(data[columnName])
        df[columnName] = serie

    # reorder columns
    for i in range(NMax):
        columns.append("load_{}".format(i + 1))

    columns.append("_long")

    for i in range(NMax - 1):
        columns.append("d_{}".format(i+1))

    df = df[columns]

    return df


if __name__ == '__main__':
    df = read_dataframe_from_nswd_file(sys.argv[1])
    print(df.head())
    df.to_csv(sys.argv[2], index=False)
