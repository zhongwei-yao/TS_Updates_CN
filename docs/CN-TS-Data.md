Times series data updates
================
Zhongwei Yao
2023-06-08

<!--chapter:end:index.Rmd-->

# 1 Stock market returns

## 1.1 File path

``` r
market_pfmc_m_path <- "/Volumes/Samsung_T7/Research/Database/CSMAR/股票市场系列/股票市场交易/综合市场交易数据/TRD_Cnmont1990-12 至 2023-03.xlsx"
```

## 1.2 Monthly returns

``` r
mkt_pfmc_m <- readxl::read_excel(market_pfmc_m_path) %>% 
  setlowercolnames() %>% 
  #< 117: 沪深京A股和创业板和科创板; 53: 沪深A股和创业板和科创板
  filter(markettype == 117) %>% 
  mutate(tradingmonth = ymd(str_c(trdmnt, "-01"))) %>% 
  select(tradingmonth, cmretwdos) %>% 
  mutate(cmretwdos = as.double(cmretwdos)) %>% 
  na.omit()
```

### 1.2.1 Cumulative MRET: Backward

``` r
marketret_m_ds <- mkt_pfmc_m
for (i in seq(1, 59, 1)){
  m <- i + 1
  marketret_m_ds <- bind_cols(
    marketret_m_ds,
    tibble(
      "marketret_m_backward_{{m}}m" := slide_period(mkt_pfmc_m$cmretwdos, mkt_pfmc_m$tradingmonth, .period = "month", .before = i, .complete = T, .f = ~prod(1+.x) - 1)
    )
  )
}
```

### 1.2.2 Cumulative MRET: Forward

``` r
for (i in seq(1, 60, 1)){
  marketret_m_ds <- bind_cols(
    marketret_m_ds,
    tibble(
      "marketret_m_forward_{{i}}m" := slide_period(mkt_pfmc_m$cmretwdos, mkt_pfmc_m$tradingmonth, .period = "month", .before = -1, .after = i, .complete = T, .f = ~prod(1+.x) - 1)
    )
  )
}

marketret_m_csmar <- unnest(marketret_m_ds, everything()) %>% 
  rename(marketret_m_backward_1m = cmretwdos) %>% 
  mutate(tradingmonth = ceiling_date(tradingmonth, "m") -days(1))
```

<!--chapter:end:01-MRET.Rmd-->

# 2 Market volatility

## 2.1 File path

``` r
market_pfmc_d_path <- "/Volumes/Samsung_T7/Research/Database/CSMAR/股票市场系列/股票市场交易/综合市场交易数据/TRD_Cndalym1990-12-19 至 2023-04-20.csv"
```

## 2.2 Stock market variance

股票方差是过去252个交易日A股市场加权市场投资组合日收益率平方

``` r
market_svar_m_csmar <- data.table::fread(market_pfmc_d_path) %>% 
  tibble() %>% 
  setlowercolnames() %>% 
  filter(markettype == 117) %>% 
  select(tradingdate = trddt, cdretwdos) %>% 
  mutate(tradingdate = ymd(tradingdate), svar = cdretwdos * cdretwdos) %>% 
  na.omit() %>% 
  arrange(tradingdate) %>% 
  mutate(svar = rollsumr(svar, 252, fill = NA)) %>% 
  group_by(year(tradingdate), month(tradingdate)) %>% 
  summarise(tradingdate = last(tradingdate), svar = last(svar)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(tradingmonth = ceiling_date(tradingdate, "m") - days(1)) %>% 
  select(tradingmonth, svar)
```

<!--chapter:end:02-MVOL.Rmd-->

# 3 Market turnover

## 3.1 File path

``` r
stock_ret_month_path <- "/Volumes/Samsung_T7/Research/Database/CSMAR/股票市场系列/股票市场交易/个股回报率/月个股回报率/TRD_Mnth1990-12 至 2023-03.txt"
```

## 3.2 Monthly trading volume: All A shares

Bottom-up average with all a shares

``` r
market_turnover_m_csmar <- fread(stock_ret_month_path, colClasses = c(Stkcd = "character")) %>% 
  setlowercolnames() %>% 
  .[markettype %in% c(1, 4, 16, 32)] %>% 
  .[, tradingmonth := ymd(paste0(trdmnt, "-01"))] %>% 
  .[, .(stkcd, tradingmonth, mclsprc, mnshrtrd, mnvaltrd, msmvosd)] %>% 
  .[, mnshrfloata := msmvosd * 1000 / mclsprc] %>% 
  .[, `:=`(turnover_1 = mnshrtrd / mnshrfloata, turnover_2 = mnvaltrd / (msmvosd * 1000))] %>% 
  na.omit() %>% 
  .[, lapply(.SD, function(x) weighted.mean(x,w = msmvosd)), .SDcols = c("turnover_1", "turnover_2"), tradingmonth] %>% 
  setorder(tradingmonth) %>% 
  .[, (c("turnover_12m_mean_backward_1", "turnover_12m_backward_2")) := lapply(.SD, RcppRoll::roll_meanr, n = 12), .SDcols = c("turnover_1", "turnover_2")] %>% 
  .[, (c("turnover_12m_mean_forward_1", "turnover_12m_mean_forward_2")) := lapply(.SD, RcppRoll::roll_meanl, n = 12), .SDcols = c("turnover_1", "turnover_2")] %>% 
  .[, tradingmonth := ceiling_date(tradingmonth, "m") -days(1)] %>% 
  .[]
```

<!--chapter:end:03-MTO.Rmd-->

# 4 Inflation

## 4.1 File path

``` r
cpi_path <- "/Volumes/Samsung_T7/Research/Database/CSMAR/经济研究系列/宏观经济/价格指数/居民消费价格分类指数月度文件/CME_Mpi12003-02 至 2023-02.xlsx"
```

## 4.2 Monthly CPI month-to-month

Datasgn $$数据标识$$ - PYM＝上年同月为基期, PYP＝上年同期为基期. Areasgn
$$地区标识$$ - 1=全国，2=城镇，3=农村 Epim0101 $$居民消费价格指数$$ -
Epim0102 $$居民消费价格指数-食品$$ - Epim0103
$$居民消费价格指数-烟酒及用品$$ - Epim0104 $$居民消费价格指数-衣着$$ -
Epim0105 $$居民消费价格指数-家庭设备用品及服务$$ - Epim0106
$$居民消费价格指数-医疗保健及个人用品$$ - Epim0107
$$居民消费价格指数-交通和通信$$ - Epim0108
$$居民消费价格指数-娱乐教育文化用品及服务$$ - Epim0109
$$居民消费价格指数-居住$$ -

``` r
cpi_m_csmar <- readxl::read_xlsx(cpi_path) %>% 
  slice(3:n()) %>% 
  filter(Areasgn == 1, Datasgn == "PYM") %>% 
  mutate(tradingmonth = ceiling_date(ymd(paste0(Staper, "-01")), "m") - days(1), 
         year = year(tradingmonth),
         cpi = as.double(Epim0101)) %>% 
  # filter(month(tradingmonth) == 12, !is.na(cpi), year >= 2000) %>% 
  select(tradingmonth, cpi) %>% 
  mutate(inflation = cpi - 100) %>% 
  #< Because inflation information is released only in the following month, we wait for one month before using it in our monthly regression.
  mutate(inflation = dplyr::lag(inflation, 1)) %>%
  na.omit() %>% 
  select(-cpi)
```

<!--chapter:end:04-INFL.Rmd-->

# 5 Net equity expansion

## 5.1 File path

``` r
ntis_path <- "/Volumes/Samsung_T7/Research/Database/CSMAR/专题研究系列/事件研究/公司事件/IPO事件表/ER_IPO.txt"
```

## 5.2 NTIS

过去12个月新股募资金额/当月流通A股市值

``` r
ntis_m_csmar <- fread(ntis_path) %>% 
  .[CurrencyCode == "CNY", .(raisefund = RaiseFund, enddate = fifelse(is.na(EndDate), ListedDate, StartDate))] %>% 
  .[, tradingmonth := floor_date(enddate, "m")] %>% 
  .[, .(raisefund = sum(raisefund, na.rm = T)), tradingmonth] %>% 
  .[data.table(tradingmonth = seq.Date(ymd("2000-1-1"), ymd("2023-1-1"), by = "month")), on = "tradingmonth"] %>% 
  setorder(tradingmonth) %>% 
  .[, ntis :=  roll_sumr(raisefund, n = 12, fill = NA, na.rm = T)] %>% 
  .[, .(tradingmonth, ntis)] %>% 
  left_join(
    readxl::read_excel(market_pfmc_m_path) %>% 
      setlowercolnames() %>% 
      filter(markettype == 117) %>% 
      mutate(tradingmonth = ymd(str_c(trdmnt, "-01"))) %>% 
      select(tradingmonth, cmmvosd) %>% 
      na.omit()
  ) %>% 
  as.data.table() %>% 
  na.omit() %>% 
  .[, .(tradingmonth = ceiling_date(tradingmonth, "m") - days(1), ntis = ntis / (cmmvosd * 1000))] 
```

<!--chapter:end:05-NTIS.Rmd-->

# 6 Government bond yields

中债到期收益率曲线

``` r
gb_path <- "/Volumes/Samsung_T7/Research/Database/中债/中债国债到期收益率标准期限信息/"
```

## 6.1 中债到期收益率曲线

``` r
gb_ts <- paste0(gb_path, dir(gb_path, ".xlsx")) %>% 
  map_dfr(
    ~readxl::read_xlsx(.x) %>% 
      select(yield = `收益率(%)`, tenure = `标准期限说明`, tradingdate = `日期`) %>% 
      mutate(yield = as.double(yield), tradingdate = ymd(tradingdate), tradingmonth = ceiling_date(tradingdate, "m") - days(1))
  ) %>% 
  group_by(tradingmonth, tenure) %>% 
  summarise_all(last) %>% 
  ungroup() %>% 
  select(-tradingdate)
```

## 6.2 Short-term yield: 3m

``` r
sty_3m_m_cb <- gb_ts %>% 
  filter(tenure == "3m") %>% 
  select(-tenure) %>% 
  rename(sty = yield)
```

## 6.3 Long-term yield: 10Yr

``` r
lty_10yr_m_cb <- gb_ts %>% 
  filter(tenure == "10y") %>% 
  select(-tenure) %>% 
  rename(lty = yield)
```

## 6.4 Termspread

``` r
termspread_m_cb <- lty_10yr %>% 
  left_join(sty_3m) %>% 
  mutate(termspread = lty - sty) %>% 
  select(tradingmonth, termspread)
```

<!--chapter:end:06-GBY.Rmd-->

# 7 Market valuation

``` r
mv_path <- "/Volumes/Samsung_T7/Research/Database/WIND/指数/指数行情序列/STK_INDEX_VALUATION_update202302.xlsx"
```

## 7.1 D/P, E/P, B/M

``` r
market_valuation_m_wind <- readxl::read_xlsx(mv_path, "WINDA") %>% 
      mutate(tradingmonth = ymd(ceiling_date(ymd, "month") - days(1)), ep_winda = 1 / pe, bp_winda = 1 / pb, dp_winda = dp) %>% 
      select(tradingmonth, ep_winda, bp_winda, dp_winda, pe_winda = pe, pb_winda = pb) 
```

<!--chapter:end:07-VALUATION.Rmd-->

# 8 Merge data

``` r
econ_var_m <- marketret_m_csmar %>% 
  left_join(market_svar_m_csmar) %>% 
  left_join(market_turnover_m_csmar) %>% 
  left_join(cpi_m_csmar) %>% 
  left_join(ntis_m_csmar) %>% 
  left_join(sty_3m_m_cb) %>% 
  left_join(lty_10yr_m_cb) %>% 
  left_join(termspread_m_cb) %>% 
  left_join(market_valuation_m_wind) %>% 
  as.data.table()
```

<!--chapter:end:99-MERGE.Rmd-->
