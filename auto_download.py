"""
Created on Tue Aug 09 10:59:05 2016

@author: Han Hong
"""

import ftplib
import re
import numpy as np
import sys
import os
import gzip
from glob import glob
from datetime import datetime, timedelta


def filter_stale_files(filenames, trading_date):
    # filter stale files, to prevent holidays, we keep at least 2 files for each contract.
    remain_file_amount = 4
    contracts = set([f.split('-')[1] for f in filenames])

    remain_files = []
    for contract in contracts:
        regex = re.compile("jason\.shaffer@tradingtechnologies\.com-" + contract + "-N[0-9]*\.csv\.gz")
        files = filter(regex.match, filenames)
        if len(files) > remain_file_amount:
            files.sort(key = lambda x: int(x.replace('-', '.').split('.')[-3][1:]))
            remain_files.extend(files[-remain_file_amount :])
        else:
            remain_files.extend(files)
    return remain_files
    

def get_today_files(trading_date, remain_attemps = 3):
    print "Try get today's file list. Remain attemps: " + str(remain_attemps)    
    try:
        ftp = ftplib.FTP("tickhistory-ftp.thomsonreuters.com")
        ftp.login("jason.shaffer@tradingtechnologies.com", "TT12345678")
        ftp.cwd("results")
        filenames = ftp.nlst()
        
        # filter out weekly data and confirmation data
        regex = re.compile("jason\.shaffer@tradingtechnologies\.com-[A-Z]*[0-9]-N[0-9]*\.csv\.gz")
        filenames = filter(regex.match, filenames)
        filenames = filter_stale_files(filenames, trading_date)
        
        # filter trading day data
        file_dates = [datetime.strptime(ftp.sendcmd('MDTM ' + f).split(' ')[-1][:8], '%Y%m%d').date() for f in filenames]
        today_files = np.array(filenames)[np.array(file_dates) == trading_date]
        ftp.quit()
        print "Files to download:"
        print today_files
        return today_files
        
    except ftplib.all_errors:
        if remain_attemps > 0:
            return get_today_files(remain_attemps - 1)
        else:
            print "ERROR: Failed to get today's file list."
            return None
            

def download_single_file(output_folder, ori_filename, new_filename, data_date, year, remain_attemps = 3):
    print "Try to download: " + new_filename + ". Remain attemps: " + str(remain_attemps)
    try:
        ftp = ftplib.FTP("tickhistory-ftp.thomsonreuters.com")
        ftp.login("jason.shaffer@tradingtechnologies.com", "TT12345678")
        ftp.cwd("results")
        
        ftp.retrbinary("RETR " + ori_filename, open(output_folder + new_filename, 'wb').write)
        ftp.quit()
        print new_filename + " downloaded"
        
    except ftplib.all_errors:
        if remain_attemps > 0:
            return download_single_file(output_folder, ori_filename, data_date, year, remain_attemps - 1)
        else:
            print "Download Failed: " + new_filename
            print "Please restart."


def download(output_folder, data_date, year):
    trading_date = datetime.strptime(year + data_date, '%Y%b%d').date()
    today_files = get_today_files(trading_date)

    # download files
    if len(today_files) > 0:
        for f in today_files:
            new_filename = f.replace('-', '.').split('.')[3] + '_' + data_date + '.csv.gz'
            if len(glob(output_folder + new_filename)) == 0:
                download_single_file(output_folder, f, new_filename, data_date, year)
        print "Download complete."
    else:
        print "Today's file list is empty."


def unzip(f):
    if f.endswith('.gz'):
        
        inFile = gzip.GzipFile(f, 'rb')
        outFile = file(f[: -3], 'wb')
        for l in inFile:
            outFile.writelines(l)
        inFile.close()
        outFile.close()
        
        """
        with gzip.open(f, 'rb') as fin, open(f[: -3],'wb') as fout:
            csv_reader = csv.reader(fin)
            csv_writer = csv.writer(fout)
            csv_writer.writerows(csv_reader)
        """
        print "Unzipped " + f

def upzip_all(folder):
    print "Unzipping files."
    urls = glob(folder + '*.gz')
    for url in urls:
        unzip(url)
    
def delete_zip_files(folder):
    urls = glob(folder + '*.gz')
    for url in urls:
        os.remove(url)


if __name__ == "__main__":
    print "########################################################################"
    print "Downloading Latest weekday's file."
    print "If the latest weekday is a holiday, please run:"
    print "python auto_download.py [year] [month_abbreviation] [date]"
    print "E.G., python auto_download.py 2016 Aug 8"
    print "Note: Latest trading day should be within 4 calendar days from today."
    print "########################################################################"
    
    ########## PARAMETERS ##########
    output_folder = ".\\ToProcess\\"
    ########## PARAMETERS ##########
    
    if len(sys.argv) not in (1, 4):
        print "ERROR: Wrong number of arguments."
    elif len(sys.argv) == 1:
        gmt_time = datetime.utcnow()
        file_date = (gmt_time - timedelta(hours = 23)).date()
        if file_date.weekday() > 4:
            file_date = file_date - timedelta(days = file_date.weekday() - 4)
        file_year = file_date.strftime("%Y")
        file_month = file_date.strftime("%b")
        file_day = file_date.strftime("%d")
        file_day = file_day if file_day[0] != '0' else file_day[1:]
        print "Last trading day: " + file_month + file_day + " " + file_year + " (" + file_date.strftime("%a") + ")"
        download(output_folder, file_month + file_day, file_year)
        #upzip_all(output_folder)
        #delete_zip_files(output_folder)
    else:
        file_year, file_month, file_day = sys.argv[1:]
        file_day = file_day if file_day[0] != '0' else file_day[1:]
        file_date = datetime.strptime(file_year + file_month + file_day, "%Y%b%d")
        print "Last trading day: " + file_month + file_day + " " + file_year + " (" + file_date.strftime("%a") + ")"
        download(output_folder, file_month + file_day, file_year)
        #upzip_all(output_folder)
        #delete_zip_files(output_folder)























































