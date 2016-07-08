/*
 
## FUNCTIONS IN CPP
#### Indicators ####
# Trend()
 
#### BackTest Functions #####
# GenerateSignals aka ApplyStop in Amibroker
# GenerateTrades 
 
#### Utility Functions#####
# ExRem
# Flip
# BarsSince
 
 */

#include <Rcpp.h>
#include <cmath>


using namespace Rcpp;
using namespace std;


NumericVector Shift(NumericVector vec1,NumericVector vec2, int ref){
        int nSize=vec1.size();
        NumericVector result(nSize);
        for(int i=1;i<nSize;i++){
                bool trigger = (vec2[i] != vec2[i - 1]) & (vec2[i] == ref);
                if((vec1[i]!=vec1[i-1])|trigger){
                        result[i]=vec1[i-1];
                }else{
                        result[i]=result[i-1];
                }
        }
        return result;
}

// [[Rcpp::export]]
DataFrame CalculateEquityCurve(String symbol,DataFrame all, DataFrame trades,NumericVector size, double brokerage){
        //This calculates equity for a single security.
        int nSize=all.nrows();
        int nTrades=trades.nrows();
        const DatetimeVector timestamp=all["date"];
        const NumericVector close=all["close"];
        const DatetimeVector entrytime=trades["entrytime"];
        const DatetimeVector exittime=trades["exittime"];
        const StringVector symbols=trades["symbol"];
        //const StringVector trade=trades["trade"];
        std::vector<std::string> trade = Rcpp::as<std::vector<std::string> >(trades["trade"]);
        const NumericVector entryprice=trades["entryprice"];
        const NumericVector exitprice=trades["exitprice"];
        NumericVector profit(nSize);
        NumericVector value(nSize);
        NumericVector contracts(nSize);
        NumericVector exposure(nSize);
        NumericVector brokeragecost(nSize);
        //std::string fmt =Rcpp::as<std::string>( format );
        //value[i]=i>0?value[i-1]:0;
        //contracts[i]=i>0?size[i-1]:0;
        //Build Positions and exposure
        int entrysize=0;
        for(int i=0;i<nSize;i++){
                if(i>0){
                        contracts[i]=contracts[i-1];
                        value[i]=value[i-1];
                        brokeragecost[i]=brokeragecost[i-1];
                }
                if(i<0){
                        Rcout << " bar:"<<i<<std::endl;
                }
                for(int j=0;j<nTrades;j++){
                        
                        if(entrytime[j]>=timestamp[0]){ //only process if the trade entry is after the first timestamp provided
                                if((entrytime[j]==timestamp[i]) & (symbols[j]==symbol)){
                                        value[i]=(trade[j].find("Short")==string::npos)?(value[i]-(size[i]*entryprice[j])):(value[i]+(size[i]*entryprice[j]));
                                        contracts[i]=(trade[j].find("Short")==string::npos)?contracts[i]+size[i]:contracts[i]-size[i];
                                        brokeragecost[i]=brokeragecost[i]+brokerage*size[i]*entryprice[j];
                                        entrysize=size[i];
                                        if(i<0){
                                                Rcout << " Entry bar:"<<i << " ,trade bar:"<<j<<" ,trade:" <<trade[j] <<" ,current contracts:"<< contracts[i]<<+
                                                        " ,new value:" <<value[i] << " ,size:" <<size[i] <<" ,BrokerageCost:"<<brokeragecost[i]<<std::endl;
                                        }
                                } 
                                if ((exittime[j]==timestamp[i]) & (symbols[j]==symbol)){
                                        value[i]=(trade[j].find("Long")==string::npos)?(value[i]-(entrysize*exitprice[j])):(value[i]+(entrysize*exitprice[j]));
                                        contracts[i]=(trade[j].find("Long")==string::npos)?contracts[i]+entrysize:contracts[i]-entrysize;
                                        brokeragecost[i]=brokeragecost[i]+brokerage*entrysize*exitprice[j];
                                        if(i<0){
                                                Rcout << " Exit bar:"<<i <<" ,trade bar:" << j <<" ,trade:"<<trade[j] <<" ,current contracts:"<<contracts[i]<<+
                                                        " ,new value:" <<value[i] << " ,size:" <<entrysize << " ,BrokerageCost:"<<brokeragecost[i]<<std::endl;
                                        }
                                        entrysize=0;
                                }
                        }
                }
        }
        //mtm positions
        for(int i=0;i<nSize;i++){
                profit[i]=value[i]+contracts[i]*close[i]-brokeragecost[i];// 
                exposure[i]=abs(contracts[i])*close[i];
        }
        
        return DataFrame::create(_["date"]=timestamp,_["marketdues"]=value,_["contracts"]=contracts,_["profit"]=profit,_["close"]=close,_["brokerage"]=brokeragecost,_["exposure"]=exposure);
        
}




// [[Rcpp::export]]
DataFrame GenerateTrades(DataFrame all){
        int nSize=all.nrows();
        const NumericVector buy=all["buy"];
        const NumericVector sell=all["sell"];
        const NumericVector shrt=all["short"];
        const NumericVector cover=all["cover"];
        const NumericVector buyprice=all["buyprice"];
        const NumericVector sellprice=all["sellprice"];
        const NumericVector shortprice=all["shortprice"];
        const NumericVector coverprice=all["coverprice"];
        const CharacterVector symbol=all["symbol"];
        const DatetimeVector timestamp=all["date"];
        LogicalVector buyprocessed(nSize);
        LogicalVector sellprocessed(nSize);
        LogicalVector shortprocessed(nSize);
        LogicalVector coverprocessed(nSize);
        
        //calculate # of trades
        int tradecount=0;
        for(int i=0;i<nSize;i++){
                if(buy[i]>0){
                        tradecount++;
                }
                if(shrt[i]>0){
                        tradecount++;
                }
        }
        
        CharacterVector tradesymbol(tradecount);
        StringVector trade(tradecount);
        DatetimeVector entrytime(tradecount);
        NumericVector entryprice(tradecount);
        DatetimeVector exittime(tradecount);
        NumericVector exitprice(tradecount);
        NumericVector percentprofit(tradecount);
        NumericVector bars(tradecount);
        Function formatDate("format.POSIXct");
        int tradesize=-1;
        for(int i=0;i<nSize;i++){
                int entrybar=0;
                if((buyprocessed[i]==false) & (buy[i]>0)){
                        tradesize++;
                        tradesymbol[tradesize]=symbol[i];
                        if(buy[i]==1){
                                trade[tradesize]="Long";
                        }else if(buy[i]==2){
                                trade[tradesize]="ReplacementLong";
                        }
                        entrytime[tradesize]=timestamp[i];  
                        entryprice[tradesize]=buyprice[i];
                        entrybar=i;
                        buyprocessed[i]=true;
                        int j=i+1;
                        while ((j<nSize) & (sellprocessed[j]==false) & (sell[j]==0)) {
                                sellprocessed[j]=true;  
                                j++;
                        }
                        if(j<nSize){
                                sellprocessed[j]=true;
                                exittime[tradesize]=timestamp[j]; 
                                exitprice[tradesize]=sellprice[j];
                                bars[tradesize]=j-entrybar;
                                percentprofit[tradesize]=(exitprice[tradesize]-entryprice[tradesize])/entryprice[tradesize];
                                //Rcout << " Found Sell. Bar: " <<i<<" ,subbar: "<<j<< " ,Sell[subbar] :" << sell[j] << " ,entrybar: "<<entrybar<<std::endl;
                        }
                }else if((shortprocessed[i]==false) & (shrt[i]>0)){
                        tradesize++;
                        tradesymbol[tradesize]=symbol[i];
                        if(shrt[i]==1){
                                trade[tradesize]="Short";
                        }else if(shrt[i]==2){
                                trade[tradesize]="ReplacementShort";
                        }
                        entrytime[tradesize]=timestamp[i]; 
                        entryprice[tradesize]=shortprice[i];
                        entrybar=i;
                        shortprocessed[i]=true;
                        int j=i+1;
                        while ((j<nSize) & (coverprocessed[j]==false) & (cover[j]==0)) {
                                coverprocessed[j]=true;  
                                j++;
                        }
                        if(j<nSize){
                                coverprocessed[j]=true;
                                exittime[tradesize]=timestamp[j]; 
                                exitprice[tradesize]=coverprice[j];
                                bars[tradesize]=j-entrybar;
                                percentprofit[tradesize]=-(exitprice[tradesize]-entryprice[tradesize])/entryprice[tradesize];
                        }
                }
                
                // if(shortprocessed[i]==false){
                //   shortprocessed[i]=true;
                //   shortprice=0;
                // }
                // if(coverprocessed[i]==false){
                //   coverprocessed[i]=true;
                //   coverprice=0;
                // }
                // if(buyprocessed[i]==false){
                //   buyprocessed[i]=true;
                //   buyprice=0;
                // }
                // if(sellprocessed[i]==false){
                //   sellprocessed[i]=true;
                //   sellprice=0;
                // }
        }
        return DataFrame::create(_["symbol"]=tradesymbol,_["trade"]=trade,_["entrytime"]=entrytime,
                                 _["entryprice"]=entryprice,_["exittime"]=exittime,_["exitprice"]=exitprice,
                                 _["percentprofit"]=percentprofit,_["bars"]=bars, _["stringsAsFactors"] = false
        );
}



// [[Rcpp::export]]
DataFrame GenerateSignals(const DataFrame all,NumericVector amount){
        //stop mode can be 1: points
        int nSize=all.nrows();
        const NumericVector inlongtrade=all["inlongtrade"];
        const NumericVector inshorttrade=all["inshorttrade"];
        const NumericVector open=all["open"];
        const NumericVector high=all["high"];
        const NumericVector low=all["low"];
        const NumericVector close=all["close"];
        const NumericVector buy=all["buy"];
        const NumericVector sell=all["sell"];
        const NumericVector shrt=all["short"];
        const NumericVector cover=all["cover"];
        const StringVector symbol=all["symbol"];
        
        const NumericVector buyprice=all["buyprice"];
        const NumericVector sellprice=all["sellprice"];
        const NumericVector shortprice=all["shortprice"];
        const NumericVector coverprice=all["coverprice"];
        const DatetimeVector timestamp=all["date"];
        
        NumericVector lbuy(nSize);
        NumericVector lsell(nSize);
        NumericVector lshrt(nSize);
        NumericVector lcover(nSize);
        
        NumericVector lbuyprice(nSize);
        NumericVector lsellprice(nSize);
        NumericVector lshortprice(nSize);
        NumericVector lcoverprice(nSize);
        
        const NumericVector damount=amount;
        
        bool stoplosstriggered =false;
        int barstart=0;
        for(int i=0;i<nSize;i++){
                if(buy[i]>0){
                        lbuy[i]=buy[i];
                        lbuyprice[i]=buyprice[i];
                }
                if(shrt[i]>0){
                        lshrt[i]=shrt[i];
                        lshortprice[i]=shortprice[i];
                }
                if(sell[i]>0){
                        lsell[i]=sell[i];
                        lsellprice[i]=sellprice[i];
                }
                if(cover[i]>0){
                        lcover[i]=cover[i];
                        lcoverprice[i]=coverprice[i];
                }
        }
        for(int i=1;i<nSize;i++){
                stoplosstriggered=false;
                bool newtrade=(lbuy[i-1]>0)|(lshrt[i-1]>0);
                //Rcout << "The value NewTrade at i: " << i <<" is "<< newtrade << ", lbuy[i-1]: "<<lbuy[i-1] <<" ,shrt[i-1]"<<shrt[i-1] <<std::endl;
                if(newtrade){ //reset stoplosstriggered flag.
                        stoplosstriggered=false;
                        barstart=i-1;
                }
                
                if(!stoplosstriggered){//stoplossnottriggered
                        if(inlongtrade[i-1]==1){
                                //check if stoploss triggered for a long trade
                                double slprice=buyprice[barstart]-damount[barstart];
                                //Rcout << "The value sl at i: " << i <<" is "<< slprice << ", barstart: "<<barstart <<" ,ref buyprice:"<<buyprice[barstart]<<" ,loss amt: "<<damount[barstart] <<std::endl;
                                if(open[i]<=slprice){
                                        lsellprice[i]=open[i];
                                        stoplosstriggered=true;
                                        lsell[i]=3;
                                }else if ((low[i]<=slprice) & (high[i]>=slprice)){
                                        lsellprice[i]=slprice;
                                        stoplosstriggered=true;
                                        lsell[i]=2;//maxsl
                                        //Rcout << "SL triggered at i: " << i <<" Trigger price is"<< slprice <<std::endl;
                                }
                        }else if(inshorttrade[i-1]==1){
                                //check if stoploss triggered for a short trade
                                double slprice=shortprice[barstart]+damount[barstart];
                                if(open[i]>=slprice){
                                        lcoverprice[i]=open[i];
                                        stoplosstriggered=true;
                                        lcover[i]=3;
                                }else if ((low[i]<=slprice) & (high[i]>=slprice)){
                                        lcoverprice[i]=slprice;
                                        stoplosstriggered=true;
                                        lcover[i]=2;
                                        //Rcout << "SL triggered at i: " << i <<" Trigger price is"<< slprice <<std::endl;
                                }
                        }
                }
                
                if(stoplosstriggered){//check if a replacement trade is needed
                        //needed if intrade is true at the bar
                        if(inlongtrade[i]==1 && cover[i]==0 ){
                                //enter fresh long trade. Update all buyprices for bars ahead, till you arrive at a non-zero buyprice.
                                int j=i;
                                lbuy[i]=2;//replacement trade
                                stoplosstriggered=false;
                                do {
                                        lbuyprice[j]=close[i];
                                        j++;
                                } while(inlongtrade[j]!=0);
                                
                        }else if((inshorttrade[i]==1) & (sell[i]==0)){
                                //enter fresh short trade
                                int j=i;
                                stoplosstriggered=false;
                                lshrt[i]=2;//replacement trade
                                do{
                                        lshortprice[j]=close[i];
                                        j++;
                                }while(inshorttrade[j]!=0);
                        }
                }
        }
        return DataFrame::create(_["date"]=timestamp,_["symbol"]=symbol,_["buy"]=lbuy,_["sell"]=lsell,_["short"]=lshrt,_["cover"]=lcover,
                                 _["buyprice"]= lbuyprice, _["sellprice"]= lsellprice,_["shortprice"]=lshortprice,_["coverprice"]=lcoverprice);
        
}

// [[Rcpp::export]]
NumericVector ExRem(NumericVector vec1, NumericVector vec2){
        int nSize=vec1.size();
        NumericVector result(nSize);
        bool vec2received=false;
        for(int i=1;i<nSize;i++){
                
                if((vec1[i]>0 && vec2received)){
                        result[i]=vec1[i];
                        vec2received=false;
                }
                if(vec2[i]>0 && result[i]>0){//we received a positive in vec1 in the same bar as positive in vec2
                        //both vecs neutralized.
                        result[i]=0;
                        vec2received=true; // if buy and sell occur together, we dont select buy signal, but will allow next buy signal to pass.
                }else if(vec2[i]>0){
                        vec2received=true;
                }
                
        }
        return result;
}

// [[Rcpp::export]]
NumericVector Flip(NumericVector vec1, NumericVector vec2){
        NumericVector result=ExRem(vec1,vec2);
        for(int i=1;i<result.size();i++){
                if(result[i-1]>0 && vec2[i]==0){
                        result[i]=result[i-1];
                }
        }
        return result;
}


// [[Rcpp::export]]
NumericVector BarsSince(NumericVector vec1){
        int nSize=vec1.size();
        NumericVector result(nSize);
        for(int i=0;i<nSize;i++){
                if (vec1[i]==1){
                        result[i]=0;
                }else{
                        result[i]=result[i-1]+1;
                }
        }
        return result;
}

// [[Rcpp::export]]
DataFrame Trend(DatetimeVector date,NumericVector high,NumericVector low, NumericVector close){
        //int startIndex = 1;
        int swingLowStartIndex = 0;
        int swingHighStartIndex = 0;
        //int swingLowEndIndex;
        //int swingHighEndIndex;
        int nSize=close.size();
        NumericVector result(nSize); 
        
        NumericVector updownbarclean(nSize); 
        NumericVector updownbar(nSize); 
        NumericVector outsidebar(nSize); 
        NumericVector insidebar(nSize); 
        
        NumericVector swinghigh(nSize);        
        NumericVector swinghighhigh(nSize);
        NumericVector swinghighhigh_1(nSize);
        NumericVector swinghighhigh_2(nSize);
        
        
        NumericVector swinglow(nSize);
        NumericVector swinglowlow(nSize); 
        NumericVector swinglowlow_1(nSize); 
        NumericVector swinglowlow_2(nSize); 
        
        
        NumericVector swinglevel(nSize); 
        
        
        for (int i = 1; i < nSize; i++) {
                result[i] = 0;
                updownbarclean[i] = 0;
                updownbar[i] = 0;
                outsidebar[i] = 0;
                insidebar[i] = 0;
                
                bool hh = high[i] > high[i - 1];
                bool lh = high[i] < high[i - 1];
                bool ll = low[i] < low[i - 1];
                bool hl = low[i] > low[i - 1];
                bool el = low[i] == low[i - 1];
                bool eh = high[i] == high[i - 1];
                
                if ((hh && hl) || (eh && hl) || (hh && el)) {
                        updownbarclean[i] = 1;
                        updownbar[i] = 1;
                }
                else if ((lh && ll) || (el && lh) || (ll && eh)) {
                        updownbarclean[i] = -1;
                        updownbar[i] = -1;
                }
                else if (hh && ll) { //outside bar
                        outsidebar[i] = 1;
                }
                else {
                        insidebar[i] = 1;
                }
        }
        
        for (int i = 1; i < nSize; i++) {
                if (outsidebar[i] == 1) {
                        updownbar[i] = -updownbar[i - 1];
                }
                else if (insidebar[i] == 1) {
                        updownbar[i] = updownbar[i - 1];
                }
        }
        
        for (int i = 1; i < nSize; i++) {
                swinglevel[i] = 0;
                swinghigh[i], swinghighhigh[i], swinghighhigh_1[i] = 0;
                swinglow[i], swinglowlow[i], swinglowlow_1[i] = 0;
                if (updownbar[i] == 1) {//we are in upswing
                        if (updownbar[i - 1] == 1) {//continuing upswing
                                swinghigh[i] = high[i]>swinghigh[i-1]?high[i]:swinghigh[i-1];
                                //swinghighhigh will be updated when the upswing ends or if its the last upswing.
                                //update swinglow and swinglowlow for "i"
                                if ((swinglow[i - 1] != 0) & (outsidebar[i] != 1)) { //prevent init of swinglow to 0!!
                                        swinglow[i] = swinglow[i - 1];
                                        swinglowlow[i] = swinglow[i - 1];
                                }
                        }
                        else { //first day of upswing
                                swingHighStartIndex = i;
                                //swingLowEndIndex = i - 1;
                                
                                swinghigh[i] = high[i];
                                if (outsidebar[i] == 1) {
                                        swinglow[i] = std::min(low[i], swinglow[i - 1]);
                                        swinglowlow[i] = swinglow[i];
                                }
                                else {
                                        swinglow[i] = swinglow[i - 1];
                                        swinglowlow[i] = swinglow[i - 1];
                                }
                                
                                for (int j = swingLowStartIndex; j < swingHighStartIndex; j++) {
                                        if (outsidebar[i] == 1) {
                                                swinglowlow[j] = swinglow[i];
                                        }
                                        else {
                                                swinglowlow[j] = swinglow[i - 1];
                                        }					
                                }
                        }
                }		else if (updownbar[i] == -1) {//we are in a downswing
                        if (updownbar[i - 1] == -1) {//continuing downswing
                                swinglow[i] = min(low[i], swinglow[i - 1]);
                                //swinglowlow will be updated when the downswing ends or if its the last downswing.
                                //update swinghigh and swinghighhigh for "i"
                                if ((swinghigh[i - 1] != 0) & (outsidebar[i] != 1)) { //prevent init of swinghigh to 0!!
                                        swinghigh[i] = swinghigh[i - 1];
                                        swinghighhigh[i] = swinghigh[i - 1];
                                }
                        }
                        else { //first day of downswing
                                swingLowStartIndex = i;
                                //swingHighEndIndex = i - 1;
                                
                                swinglow[i] = low[i];
                                if (outsidebar[i] == 1) {
                                        swinghigh[i] = max(high[i], swinghigh[i - 1]);
                                        swinghighhigh[i] = swinghigh[i];
                                }
                                else {
                                        swinghigh[i] = swinghigh[i - 1];
                                        swinghighhigh[i] = swinghigh[i - 1];
                                }
                                for (int j = swingHighStartIndex; j < swingLowStartIndex; j++) {
                                        if (outsidebar[i] == 1) {
                                                swinghighhigh[j] = swinghigh[i];
                                        }
                                        else {
                                                swinghighhigh[j] = swinghigh[i - 1];
                                        }
                                }
                        }			
                }
        }
        
        //update swinghighhigh,swinglowlow for last (incomplete) swing
        if (swingHighStartIndex > swingLowStartIndex) {//last incomplete swing is up
                for (int j = swingHighStartIndex; j < nSize; j++) {
                        swinghighhigh[j] = swinghigh[nSize - 1];
                }
        }
        else {
                for (int j = swingLowStartIndex; j < nSize; j++) {
                        swinglowlow[j] = swinglow[nSize - 1];
                }
        }
        
        //calculate shifted versions of highhigh and lowlow
        swinglowlow_1=Shift(swinglowlow,updownbar,-1);
        swinghighhigh_1 = Shift(swinghighhigh, updownbar,1);
        swinglowlow_2 = Shift(swinglowlow_1,updownbar,-1);
        swinghighhigh_2 = Shift(swinghighhigh_1,updownbar, 1);
        
        //create swing level
        for (int j = 1; j < nSize; j++) {
                if (updownbar[j] == 1) {
                        swinglevel[j] = swinghighhigh[j];
                }
                else if (updownbar[j] == -1){
                        swinglevel[j] = swinglowlow[j];
                }
                else {
                        swinglevel[j] = 0;
                }
        }
        
        
        // update trend
        for (int i = 1; i < nSize; i++) {
                result[i] = 0;
                
                bool up1 = (updownbar[i] == 1) && swinghigh[i] > swinghighhigh_1[i] && swinglow[i] > swinglowlow_1[i];
                bool up2 = (updownbar[i] == 1) && swinghighhigh_1[i] > swinghighhigh_2[i] && swinglow[i] > swinglowlow_1[i];
                bool up3 = (updownbar[i] == -1 || outsidebar[i] == 1) && swinghigh[i] > swinghighhigh_1[i] && swinglowlow_1[i] > swinglowlow_2[i] && low[i] > swinglowlow_1[i];
                bool down1 = (updownbar[i] == -1) && swinghigh[i] < swinghighhigh_1[i] && swinglow[i] < swinglowlow_1[i];
                bool down2 = (updownbar[i] == -1) && swinghigh[i] < swinghighhigh_1[i] && swinglowlow_1[i] < swinglowlow_2[i];
                bool down3 = (updownbar[i] == 1 || outsidebar[i] == 1) && swinghighhigh_1[i] < swinghighhigh_2[i] && swinglow[i] < swinglowlow_1[i] && high[i] < swinghighhigh_1[i];
                
                if (up1 || up2 || up3) {
                        result[i] = 1;
                }
                else if (down1 || down2 || down3) {
                        result[i] = -1;
                }		
        }
        
        //        DataFrame out=create()(Named("trend")=result,Named("updownbar")=updownbar);
        return DataFrame::create(_("date")=date,_("trend")=result,_("updownbar")=updownbar,
                                   _("outsidebar")=outsidebar,_("insidebar")=insidebar,
                                   _("swinghigh")=swinghigh,_("swinglow")=swinglow,
                                   _("swinghighhigh")=swinghighhigh,_("swinglowlow")=swinglowlow,
                                   _("Swinghighhigh_1")=swinghighhigh_1,_("swinglowlow_1")=swinglowlow_1,
                                   _("swinghighhigh_2")=swinghighhigh_2,_("swinglowlow_2")=swinglowlow_2,
                                   _("swinglevel")=swinglevel);
}

