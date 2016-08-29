#!/usr/local/bin/python3.4
from datetime import date
from datetime import datetime
import datetime as dt
from datetime import timedelta
import sys
import os.path
import numpy as np
import string
__out_file_directory__ = "./"
q_bids = 0
q_bid = 1
q_ask = 2
q_asks = 3
max_size = 999999

# need to test this to make sure trades on sunday are not sent to the previous sunday's file: success
def get_sunday_before(in_date):
    d = in_date.toordinal()
    sunday = d - (d % 7)
    return date.fromordinal(sunday)


def monthNameToNum(month_str):
    month = "0"
    if month_str == "JAN":
        month = "01"
    elif month_str == "FEB":
        month = "02"
    elif month_str == "MAR":
        month = "03"
    elif month_str == "APR":
        month = "04"
    elif month_str == "MAY":
        month = "05"
    elif month_str == "JUN":
        month = "06"
    elif month_str == "JUL":
        month = "07"
    elif month_str == "AUG":
        month = "08"
    elif month_str == "SEP":
        month = "09"
    elif month_str == "OCT":
        month = "10"
    elif month_str == "NOV":
        month = "11"
    elif month_str == "DEC":
        month = "12"
    return month


def quote_str(cur_mkt):
    # cur_mkt is list bid size, bid, ask, ask size
    s = str(cur_mkt[0]) + ',' + str(cur_mkt[1]) + ',' + str(cur_mkt[2]) + ',' + str(cur_mkt[3]) + ','
    return s


def q_reset(mkt):
    # cur_mkt is list bid size, bid, ask, ask size
    mkt[0] = 0
    mkt[1] = float("NaN")
    mkt[2] = float("NaN")
    mkt[3] = 0


def copy_quote(tgt, src):
    tgt[0] = src[0]
    tgt[1] = src[1]
    tgt[2] = src[2]
    tgt[3] = src[3]


def read_current_quote(line_items, hdr_list, cur_quote):
    if line_items[hdr_list.index('Ask Size')] != '':
        cur_quote[q_asks] = int(line_items[hdr_list.index('Ask Size')])
        if cur_quote[q_asks] == 0:  # then invalid
            cur_quote[q_ask] = float("NaN")
    if line_items[hdr_list.index('Bid Size')] != '':
        cur_quote[q_bids] = int(line_items[hdr_list.index('Bid Size')])
        if cur_quote[q_bids] == 0:  # then invalid
            cur_quote[q_bid] = float("NaN")
    if line_items[hdr_list.index('Ask Price')] != '':
        if cur_quote[q_asks] > 0:
            cur_quote[q_ask] = float(line_items[hdr_list.index('Ask Price')])
    if line_items[hdr_list.index('Bid Price')] != '':
        if cur_quote[q_bids] > 0:
            cur_quote[q_bid] = float(line_items[hdr_list.index('Bid Price')])


def invalid_quote(cur_mkt):
    # cur_mkt is list bid size, bid, ask, ask size
    if (float(cur_mkt[0]) == 0.0) or (float(cur_mkt[3]) == 0.0) or \
            (np.isnan(float(cur_mkt[1]))) or (np.isnan(float(cur_mkt[2]))):
        return True
    return False


def get_num_decimal_places(*args):
    '''
        This function takes in a variable number of numbers given as strings.
        It goes through each number/string to determine the number of
        decimal places. It returns the maximum number of decimal places
        over all numbers/strings.

        This was added because in the TRTH file, Reuters does not provide
        trailing zeroes, but we need the maximum number of decimals in
        order to properly report a trade-through.
    '''
    max_len = 0
    for num_string in args:
        decimal_split = str(num_string).split('.')
        if len(decimal_split) > 1:  # then there is something after a decimal point
            if len(decimal_split[1]) > max_len:
                max_len = len(decimal_split[1])
    return str(max_len)


def convert_file(exchange, file_name, output_file_name):
    # requestedHeaders is a list of the headers we would like extracted from the raw Reuters dump. This makes
    # the script a bit more nimble to changes in data requests as well as raw formats. EC Dec 2014
    requested_headers = ['#RIC', 'Date[G]', 'Time[G]', 'GMT Offset', 'Type', 'Price', 'Volume', 'Bid Price',
                         'Bid Size', 'Ask Price', 'Ask Size', 'Qualifiers']

    file_start_time = datetime.now()
    print("starting conversion on " + file_name + " at: " + str(file_start_time))
    exchange = exchange.lower()
    if exchange in ['cmx', 'nym']:
        mkt_close = dt.time(17, 15, 00)
    elif exchange in ['cme', 'imm', 'cbt']:
        mkt_close = dt.time(16, 15, 00)
    elif exchange in ['ieu']:
        mkt_close = dt.time(23, 00, 00)
    elif exchange in ['ius']:
        mkt_close = dt.time(18, 00, 00)
    elif exchange in ['lme']:
        mkt_close = dt.time(19, 00, 00)
    elif exchange in ['test']:
        mkt_close = dt.time(19, 30, 00)
    else:
        print '**** Error ****'
        print 'Exchange not found.'
        return

    with open(str(file_name)) as infile:
        # requestedIndices is a List that stores the indices of the columns we requested above in requestedHeaders
        output = open(str(output_file_name), "w")
        prior_date = ''
        prior_time = ''
        prior_local_time = dt.datetime(2000, 1, 1, 0, 0, 0)
        min_bid_size = max_size
        max_bid_size = -1
        min_ask_size = max_size
        max_ask_size = -1
        quote_at_min_ask = [0, 0, max_size, 0]
        quote_at_max_bid = [0, -1, 0, 0]
        cur_quote = [0, 0, 0, 0]
        num_requotes = 0
        min_ba_spread = max_size
        max_ba_spread = -max_size
        line_num = 0
        hdr_list = []
        qualifiers = {}

        for line in infile:
            skip_flag = False
            if line_num == 0:
                col_headers = line.split(',')
                print col_headers
                for h in col_headers:
                    #  need to test if h ends in \n
                    #  add header to list
                    hdr_list.append(h.strip())
                #  make sure that all required headers are included
                for reqd in requested_headers:
                    if reqd not in hdr_list:
                        err_txt = "** Error :  requested header " + reqd + " not present."
                        print err_txt
                        output.close()
                        infile.close()
                        return
                line_num = 1
                s = 'timestamp,local_timestamp,RIC,bid_sz,bid,ask,ask_sz,num_quotes,'
                s += 'maxbid_bid_sz,maxbid_bid,maxbid_ask,maxbid_ask_sz,'
                s += 'minask_bid_sz,minask_bid,minask_ask,minask_ask_sz,'
                s += 'min_bid_sz,max_bid_sz,min_ask_sz,max_ask_sz,'
                s += 'min_spread,max_spread\n'
                output.write(s)
                continue

            line_items = line.split(",")
            #print line

            qual = line_items[hdr_list.index('Qualifiers')]
            qual_list = []
            if qual != '' and qual != '\n':
                if qual in qualifiers.keys():
                    qualifiers[qual] += 1
                else:
                    qualifiers[qual] = 1
                qual_list = qual.split(';')

            date_pieces = line_items[hdr_list.index('Date[G]')].split("-")
            date_str = monthNameToNum(date_pieces[1]) + '/' + date_pieces[0]+'/'+date_pieces[2]
            gmt_time = line_items[hdr_list.index('Time[G]')]
            gmt_combined_str = line_items[hdr_list.index('Date[G]')] + ' ' + gmt_time
            gmt_datetime = datetime.strptime(gmt_combined_str, '%d-%b-%Y  %H:%M:%S.%f')
            gmt_offset = line_items[hdr_list.index('GMT Offset')]
            local_time = gmt_datetime + timedelta(hours=int(gmt_offset))
            #   We need to ensure that if the market has closed, we reset the current quote
            #   to avoid having any stale markets
            #   For CME, the market closes at 17:15 EST and we cannot use GMT to avoid
            #   problems with daylight savings time.
            #   If the prior time was before 17:15 and current time is past 17:15, then we know
            #   to reset curBid and curAsk to 0.0
            #   the program uses the GMT Offset, which is given in the Reuters file.
            ticker = line_items[hdr_list.index('#RIC')]
            if line_num == 1:
                if exchange in ['cme', 'cmx', 'nym'] and not ticker.startswith('1'):  # this is only for CME
                    print '*****  CME error:  must use only electronic data. Ticker does not start w/ a 1.'
                    print '*****  if electronic only and using composite, then you are okay.'

            if prior_local_time.time() < mkt_close:
                last_mkt_close = datetime.combine(prior_local_time.date(), mkt_close)
            else:
                last_mkt_close = datetime.combine(prior_local_time.date() + timedelta(days=1), mkt_close)
            if prior_local_time <= last_mkt_close < local_time:
                # theoretically we should print out the prior quote here
                # however, practically speaking, it is desirable to omit
                # the last quote of the day as it is likely to be wide
                # and devoid of information
                q_reset(cur_quote)

            if line_items[hdr_list.index('Type')] == 'Quote':
                for q in qual_list:
                    if q.strip() == 'IND[MKT_ST_IND]':
                        skip_flag = True
                    if q.strip('" ') == '[MKT_ST_IND]':
                        temp_quote = [0, 0, 0, 0]
                        read_current_quote(line_items, hdr_list, temp_quote)
                        if invalid_quote(temp_quote):
                            skip_flag = True
                if (prior_date != date_str or line_items[hdr_list.index('Time[G]')] != prior_time) and skip_flag is False:
                    # then it is either first time period or the clock just ticked
                    # if first time period, then get info.
                    # if clock tick, print out cached data and re-set all max/min stuff
                    if (prior_date != '') and (not invalid_quote(cur_quote)):
                        #  format:
                        #   gmt timestamp, est timestamp, bid size, bid, ask, ask size, # re-quotes, quote @ min bid,
                        #   quote @ max bid, quote @ min ask, quote @ max ask, min bid size,
                        #   max bid size, min ask size, max ask size, min bid-ask spread, max b-a spread,
                        #   min bid:ask ratio, max bid:ask ratio
                        gmt_combined_str = prior_date + ' ' + prior_time
                        gmt_datetime = datetime.strptime(gmt_combined_str, '%m/%d/%Y  %H:%M:%S.%f')
                        prior_trade_local_time = gmt_datetime + timedelta(hours=int(gmt_offset))
                        s = gmt_combined_str + ','
                        s += prior_trade_local_time.strftime("%H:%M:%S.%f")[:-3] + ','
                        s += ticker + ','
                        s += quote_str(cur_quote) + str(num_requotes) + ','
                        s += quote_str(quote_at_max_bid) + quote_str(quote_at_min_ask)
                        s += str(min_bid_size) + ',' + str(max_bid_size) + ','
                        s += str(min_ask_size) + ',' + str(max_ask_size) + ','
                        s += str(min_ba_spread) + ',' + str(max_ba_spread) + '\n'
                        output.write(s)

                    prior_date = date_str
                    prior_time = line_items[hdr_list.index('Time[G]')]
                    read_current_quote(line_items, hdr_list, cur_quote)
                    num_requotes = 1
                    max_bid_size = cur_quote[q_bids]
                    max_ask_size = cur_quote[q_asks]
                    if int(cur_quote[q_bids]) > 0:
                        min_bid_size = cur_quote[q_bids]
                        copy_quote(quote_at_max_bid, cur_quote)
                    else:
                        min_bid_size = max_size
                        quote_at_max_bid = [0, -1, 0, 0]
                    if int(cur_quote[q_asks]) > 0:
                        min_ask_size = cur_quote[q_asks]
                        copy_quote(quote_at_min_ask, cur_quote)
                    else:
                        min_ask_size = max_size
                        quote_at_min_ask = [0, 0, max_size, 0]
                    min_ba_spread = max_size
                    max_ba_spread = -max_size
                    if int(cur_quote[q_asks]) > 0 and int(cur_quote[q_bids]) > 0:
                        num_decimal_places = get_num_decimal_places(cur_quote[q_ask], cur_quote[q_bid])
                        fmt = "{0:." + num_decimal_places + "f}"
                        spread = float(cur_quote[q_ask]) - float(cur_quote[q_bid])
                        min_ba_spread = fmt.format(spread)
                        max_ba_spread = fmt.format(spread)
                elif skip_flag is False:
                    #  has same date & time stamp
                    #  update values but don't print
                    read_current_quote(line_items, hdr_list, cur_quote)
                    num_requotes += 1
                    if (float(cur_quote[q_ask]) < float(quote_at_min_ask[q_ask])) and int(cur_quote[q_asks]) > 0:
                        copy_quote(quote_at_min_ask, cur_quote)
                    if (float(cur_quote[q_bid]) > float(quote_at_max_bid[q_bid])) and int(cur_quote[q_bids]) > 0:
                        copy_quote(quote_at_max_bid, cur_quote)
                    if float(cur_quote[q_bids]) < float(min_bid_size):
                        min_bid_size = cur_quote[q_bids]
                    if float(cur_quote[q_bids]) > float(max_bid_size):
                        max_bid_size = cur_quote[q_bids]
                    if float(cur_quote[q_asks]) < float(min_ask_size):
                        min_ask_size = cur_quote[q_asks]
                    if float(cur_quote[q_asks]) > float(max_ask_size):
                        max_ask_size = cur_quote[q_asks]

                    if int(cur_quote[q_asks]) > 0 and int(cur_quote[q_bids]) > 0:
                        num_decimal_places = get_num_decimal_places(cur_quote[q_ask], cur_quote[q_bid])
                        fmt = "{0:." + num_decimal_places + "f}"
                        spread = float(cur_quote[q_ask]) - float(cur_quote[q_bid])
                        if float(spread) < float(min_ba_spread):
                            min_ba_spread = fmt.format(spread)
                        if float(spread) > float(max_ba_spread):
                            max_ba_spread = fmt.format(spread)
            prior_local_time = local_time
            line_num += 1
            if (line_num % 50000) == 0:
                print "lines : " + str(line_num)

            #print quote_str(cur_quote)
            #print '****\n'
            #if line_num > 90:
            #    break

        # last line will not yet be printed
        if (prior_date != '') and (not invalid_quote(cur_quote)):
            gmt_combined_str = prior_date + ' ' + prior_time
            gmt_datetime = datetime.strptime(gmt_combined_str, '%m/%d/%Y  %H:%M:%S.%f')
            prior_trade_local_time = gmt_datetime + timedelta(hours=int(gmt_offset))
            s = gmt_combined_str + ','
            s += prior_trade_local_time.strftime("%H:%M:%S.%f")[:-3] + ','
            s += ticker + ','
            s += quote_str(cur_quote) + str(num_requotes) + ','
            s += quote_str(quote_at_max_bid) + quote_str(quote_at_min_ask)
            s += str(min_bid_size) + ',' + str(max_bid_size) + ','
            s += str(min_ask_size) + ',' + str(max_ask_size) + ','
            s += min_ba_spread + ',' + max_ba_spread + '\n'
            output.write(s)
        output.close()
        infile.close()



ice_list = ["GO", "LCC", "CC", "LSU", "SB"]
lme_list = ["CA"]


if __name__ == "__main__":
    input_dir = "C:\\Users\\hhong.TRADECO\\Desktop\\stdev\\ToProcess"
    output_dir = "C:\\Users\\hhong.TRADECO\\Desktop\\stdev\\Processed"

    filenames = next(os.walk(input_dir))[2]
    # print(len(filenames))
    for f in filenames:
        if "gz" not in f:
            if "csv" in f:
                exchange = "CME"

                for val in ice_list:
                    if val in f:
                        exchange = "IUS"

                for val in lme_list:
                    if val in f:
                        exchange = "LME"

                output_file = string.split(f, ".")[0] + "_quotes.csv"
                print("Processing: " + f + " for exchange " + exchange + " output file: " + output_file)
                convert_file(exchange, input_dir + "/" + f, output_dir + "/" + output_file)