import pandas as pd
from io import BytesIO
from zipfile import ZipFile
from urllib.request import urlopen
from tqdm import tqdm
import requests
from bs4 import BeautifulSoup
import datetime
import multiprocessing

def now():
    return datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def get_df_link():
    response = requests.get("https://opentransportdata.swiss/de/ist-daten-archiv/")
    soup = BeautifulSoup(response.content, 'html.parser')

    # extract links
    links = [a['href'] for a in soup.find_all('a', href=True)]
    links = [val for val in links if val.endswith("zip")]

    df_link = pd.DataFrame({"link": links})
    df_link = df_link.iloc[1:]
    df_link['period']=df_link['link'].str[-11:-4].str.replace('e/','').str.replace('_','-').str.replace('v','')
    df_link['period']
    df_link[['year','month']] =df_link['period'].str.split('-', expand=True)

    df_link.year=df_link.year.apply(lambda x: "20"+x if len(x)==2 else x)
    df_link['year']=pd.to_numeric(df_link['year'])
    df_link['month']=pd.to_numeric(df_link['month'])
    df_link['year_month']=pd.to_datetime(df_link[['year', 'month']].assign(DAY=1)).dt.to_period('M')

    return df_link

def get_data(list_link):

    dtype={"LINIEN_ID": "string", "LINIEN_TEXT": "string", "UMLAUF_ID":"string", "HALTESTELLEN_NAME": "string"}
    train_list=["96","98","190","192","194", "196", "198"]
    df_list=[]

    for url in list_link:

        resp = urlopen(url)
        myzip = ZipFile(BytesIO(resp.read()))

        for file in myzip.namelist():
            try:
                df = pd.read_csv(myzip.open(file), on_bad_lines = 'skip', sep=";", dtype=dtype, encoding = "ISO-8859-1")
                if len(df)==0:
                    print(file,"is empty, continuing")
                else:
                    df = df[(df["VERKEHRSMITTEL_TEXT"]=="EC") & (df["LINIEN_ID"].isin(train_list))]
                    df['download_ts']=datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                    df_list.append(df)
                    print(now(), 'processed', file)

            except Exception as exc:
                print("Exception at", file)
                print (exc)
                pass

    return pd.concat(df_list)

def download_parallelize(df_link, start_year, start_month, end_year=datetime.datetime.now().year, end_month=datetime.datetime.now().month):
    path="pt_results.csv"

    df_link = df_link[df_link.year_month>=f'{start_year}-{start_month}']
    df_link = df_link[df_link.year_month<=f'{end_year}-{end_month}']

    input_list = df_link.link.to_list()
    batch_size = 5
    equally_sized_input_list = [input_list[i : i + batch_size] for i in range(0, len(input_list), batch_size)]

    print(now(),"Partitioned into", len(equally_sized_input_list), "chunks")

    with multiprocessing.Pool(len(equally_sized_input_list)) as mp_pool:
        result=mp_pool.map(get_data, equally_sized_input_list, chunksize=1)

    pd.concat(result).to_csv("pt_results.csv", index=False)
    print("wrote to",path)


# run pipeline
df_link = get_df_link()
download_parallelize(df_link=df_link, start_year=2020, start_month=12)
