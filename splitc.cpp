#include <Rcpp.h>
#include <cstdlib>
#include <iostream>
#include <queue>  
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <string>
#include <cctype>
#include <omp.h>
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cv(NumericMatrix fit, double alpha, NumericVector index){
  int n = index.size();
  std::vector<int> nodes;
  std::vector<int> nodesid;
  for(int i=0; i<n; i++){
    if(fit(i,0) > 1){
      nodes.push_back(index[i]);
      nodesid.push_back(i);
    }
  }
  //std::cout<<"nodes#"<<nodes.size()<<std::endl;
  std::unordered_map<int,int> idx_ht;
  for(int i=0; i<index.size(); i++){
    idx_ht[index[i]] = 1;
  }

  std::unordered_map<int,double> err_ht;
  
  for(int i=0; i<index.size(); i++){
    err_ht[index[i]] = fit(i,3);
  }
  std::vector<double> node_error;
  std::vector<int> n_child;
  std::vector<int> indexcopy;
  for(int i=0; i<index.size(); i++){
    indexcopy.push_back(index[i]);
  }
  std::sort(indexcopy.begin(),indexcopy.end());
  
  for(int i=index.size()-1; i>=0; i--){
    //std::cout<<indexcopy[i]<<std::endl;
    if( idx_ht[2*indexcopy[i]] > 0 ){
      idx_ht[indexcopy[i]] = idx_ht[2*indexcopy[i]] + idx_ht[2*indexcopy[i]+1];
      err_ht[indexcopy[i]] = err_ht[2*indexcopy[i]] + err_ht[2*indexcopy[i]+1];
    }
  }


  for(int i=0; i<nodes.size(); i++){
    //std::cout<<idx_ht[nodes[i]]<<std::endl;
    n_child.push_back(idx_ht[nodes[i]]);
    node_error.push_back(err_ht[nodes[i]]);
  }
  
  double minv = INT_MAX;
  int minid = -1;
  double delta;
  for(int i=0; i<nodes.size(); i++){
    if(nodes[i] != 1){
      delta = ( fit(nodesid[i],3) - node_error[i] ) / (n_child[i] - 1);
      //std::cout<<i<<":"<<delta<<std::endl;
      //std::cout<<i<<"nodeerr:"<<fit(nodesid[i],3)<<std::endl;
      //std::cout<<i<<"childreerr:"<<node_error[i]<<std::endl;
      if(delta < minv){
        minv = delta;
        minid = nodesid[i];
      }
    }
    
  }
  std::vector<double> res;
  res.push_back(minv);
  //std::cout<<minid<<std::endl;
  res.push_back(minid);
  std::unordered_map<int, int> mp;
  for(int i=0; i<index.size(); i++){
    mp[index[i]] = i;
  }
  
  //std::vector<int> res_ids(0,n);
  std::vector<int> res_ids;
  for(int i=0; i<n; i++){
    res_ids.push_back(0);
  }
  std::queue<int> q;
  q.push(minid);
  while(!q.empty()){
    int temp = q.front();
    int id = index[q.front()];
    q.pop();
    if(mp[2*id] > 0){
      q.push(mp[2*id]);
      q.push(mp[2*id+1]);
    }
    res_ids[temp] = 1;
  }
  
  for(int i=0; i<n; i++){
    if(res_ids[i] == 0){
      res.push_back(i);
    }
  }
  NumericVector out(res.size());
  for(int i=0; i<res.size(); i++){
    out[i] = res[i];
  }
  return out;
  
  
  
  // NumericVector out(nodes.size());
  // for(int i=0; i<nodes.size(); i++){
  //   out[i] = nodes[i];
  // }
  // return out;
}

// // [[Rcpp::export]]
// NumericVector cv(NumericMatrix fit, double alpha, NumericVector index, NumericVector rowid){
//   double cur_error = 0;
//   int n = rowid.size();
//   int leafcount = 0;
//   std::vector<int> nodes;
//   std::vector<int> nodesid;
//   for(int i=0; i<n; i++){
//     if(fit(rowid[i],0) > 1){
//       nodes.push_back(index[rowid[i]]);
//       nodesid.push_back(i);
//       cur_error += fit(rowid[i],3);
//     }
//     else{
//       leafcount++;
//     }
//   }
//   std::unordered_map<int,int> idx_ht;
//   for(int i=0; i<index.size(); i++){
//     idx_ht[index[i]] = 1;
//   }
//   
//   std::unordered_map<int,double> err_ht;
//   for(int i=0; i<index.size(); i++){
//     err_ht[index[i]] = fit(i,3);
//   }
//   std::vector<double> node_error(0.0,nodes.size());
//   
//   std::vector<int> n_child(0,nodes.size());
//   std::sort(index.begin(),index.end());
//   for(int i=index.size()-1; i>=0; i--){
//     if( idx_ht[2*index[i]] > 0 ){
//       idx_ht[index[i]] = idx_ht[2*index[i]] + idx_ht[2*index[i]+1];
//       err_ht[index[i]] = err_ht[2*index[i]] + err_ht[2*index[i]+1];
//     }
//   }
//   
//   for(int i=0; i<nodes.size(); i++){
//     n_child[i] = idx_ht[nodes[i]]; 
//     node_error[i] = err_ht[nodes[i]];
//   }
//   
//   double minv = INT_MAX;
//   int minid = -1;
//   double delta;
//   for(int i=0; i<nodes.size(); i++){
//     delta = ( fit(nodesid[i],3) - node_error[i]) / (n_child[i] - 1);
//     if(delta < minv){
//       minv = delta;
//       minid = i;
//     }
//   }
//   fit(minid,0) = 1;
//   std::vector<int> res;
//  
//   res.push_back(minv);
//   std::unordered_map<int, int> mp;
//   for(int i=0; i<index.size(); i++){
//     mp[index[i]] = rowid[i];
//   }
//   std::vector<int> res_ids(0,n);
//   std::queue<int> q;
//   q.push(minid);
//   while(!q.empty()){
//     int temp = q.front();
//     int id = index[q.front()];
//     q.pop();
//     if(mp[2*id] > 0){
//       res_ids[temp] = 1;
//       q.push(mp[2*id]);
//       q.push(mp[2*id+1]);
//     }
//       
//   }
//   for(int i=0; i<n; i++){
//     if(res_ids[i] == 0){
//       res.push_back(i);
//     }
//   }
//   NumericVector out(res.size());
//   for(int i=0; i<res.size(); i++){
//     out[i] = res[i];
//   }
//   return out;
//   
// }

// [[Rcpp::export]]
NumericVector splitnc(NumericMatrix y,IntegerVector x){
  std::vector<int> ux;
  for(int i=0;i<x.size();i++){
    ux.push_back(x[i]);
  }
  std::sort(ux.begin(),ux.end());
  ux.erase(std::unique(ux.begin(),ux.end()),ux.end());
  int n = ux.size();
  NumericVector out((2*n-1),0.0);
  NumericVector goodness(n-1);
  NumericVector mean(n);
  NumericVector count(n,0.0);
  NumericVector sum(n,0.0);
  int len = y.nrow();
  for(int i=0;i<len;i++){
    for(int j=0;j<n;j++){
      if(x[i] == ux[j]){
        count[j] = count[j] + 1;
        sum[j] = sum[j] + y(i,0);
        break;
      }
    }
  }
  for(int i=0;i<n;i++){
    mean[i] = sum[i] / count[i];
  }
  
  //rank X
  for(int i=0;i<n-1;i++){
    for(int j=i+1;j<n;j++){
      if(mean[i]>mean[j]){
        double tmp = mean[j];
        mean[j] = mean[i];
        mean[i] = tmp;
        double temp = ux[j];
        ux[j] = ux[i];
        ux[i] = temp;
      }
    }
  }
  
  for(int j=0;j<n-1;j++){
    double rss = 0.0;
    double wmeanleft = 0.0;
    double wmeanright = 0.0;
    double sumTrtleft = 0.0;
    double sumUntrtleft = 0.0;
    double sumTrtWtleft = 0.0;
    double sumUntrtWtleft = 0.0;
    double sumTrtright = 0.0;
    double sumUntrtright = 0.0;
    double sumTrtWtright = 0.0;
    double sumUntrtWtright = 0.0;
    for(int i=0;i<len;i++){
      int pos = std::find(ux.begin(),ux.end(),x[i]) - ux.begin();
      if(pos<=j){
        if(y(i,1) == 1.0){
          sumTrtleft = sumTrtleft + y(i,0)/y(i,2);
          sumTrtWtleft = sumTrtWtleft + 1/y(i,2);
        }
        else{
          sumUntrtleft = sumUntrtleft + y(i,0)/(1-y(i,2));
          sumUntrtWtleft = sumUntrtWtleft + 1/(1-y(i,2));
        }
      }
      else{
        if(y(i,1) == 1){
          sumTrtright = sumTrtright + y(i,0)/y(i,2);
          sumTrtWtright = sumTrtWtright + 1/y(i,2);
        }
        else{
          sumUntrtright = sumUntrtright + y(i,0)/(1-y(i,2));
          sumUntrtWtright = sumUntrtWtright + 1/(1-y(i,2));
        }
      }
    }
    if(sumTrtWtleft != 0 && sumUntrtWtleft !=0 ){
      wmeanleft =  sumTrtleft/sumTrtWtleft - sumUntrtleft/sumUntrtWtleft;
      for(int i=0;i<len;i++){
        int pos = std::find(ux.begin(),ux.end(),x[i]) - ux.begin();
        if(pos<=j){
          rss = rss + (y(i,3) - wmeanleft)*(y(i,3) - wmeanleft);
        }
      }
    }
    else{
      goodness[j] = 0;
      continue;
    }
    
    if(sumTrtWtright != 0 && sumUntrtWtright !=0 ){
      wmeanright =  -sumTrtright/sumTrtWtright + sumUntrtright/sumUntrtWtright;
      for(int i=0;i<len;i++){
        int pos = std::find(ux.begin(),ux.end(),x[i]) - ux.begin();
        if(pos>j){
          rss = rss + (y(i,3) + wmeanright)*(y(i,3) + wmeanright);
        }
      }
      goodness[j] =  1/rss ;
    }
    else{
      goodness[j] = 0;
    }
  }
  for(int i=0;i<n-1;i++){
    out[i] = goodness[i];
  }
  for(int i=n-1;i<=2*(n-1);i++){
    out[i] = ux[i-n+1];
  }
  return out;
  
}

// [[Rcpp::export]]
NumericVector node_evaluate(NumericMatrix y){
  double sumTrt = 0.0;
  double sumUntrt = 0.0;
  double sumTrtWt = 0.0;
  double sumUntrtWt = 0.0;
  int nrow = y.nrow();
  double wmean = 0.0;
  double rss = 0.0;
  for(int i=0;i<nrow;i++){
    if(y(i,1) == 1.0){
      sumTrt = sumTrt + y(i,0)/y(i,2);
      sumTrtWt = sumTrtWt + 1/y(i,2);
    }
    else{
      sumUntrt = sumUntrt + y(i,0)/(1-y(i,2));
      sumUntrtWt = sumUntrtWt + 1/(1-y(i,2));
    }
  }
  if(sumTrtWt != 0 && sumUntrtWt !=0 ){
    wmean =  sumTrt/sumTrtWt - sumUntrt/sumUntrtWt;
  }
  for(int i=0;i<nrow;i++){
    rss = rss + (y(i,3) - wmean)* (y(i,3) - wmean);
  }
  //std::cout<<rss<<std::endl;
  NumericVector out(2);
  out[0] = wmean;
  out[1] = rss;
  return out;
}
// [[Rcpp::export]]

NumericVector splitc(NumericMatrix y){
  int n = y.nrow();
  NumericVector out(2*(n-1),0.0);
  NumericVector goodness(n-1);
  NumericVector direction(n-1);
  int max;
  max=omp_get_max_threads();
  omp_set_num_threads(max);
#pragma omp parallel for
  for(int j=0;j<n-1;j++){
    double rss = 0.0;
    double wmeanleft = 0.0;
    double wmeanright = 0.0;
    double sumTrtleft = 0.0;
    double sumUntrtleft = 0.0;
    double sumTrtWtleft = 0.0;
    double sumUntrtWtleft = 0.0;
    double sumTrtright = 0.0;
    double sumUntrtright = 0.0;
    double sumTrtWtright = 0.0;
    double sumUntrtWtright = 0.0;
    // left child
    for(int i=0;i<=j;i++) {
      if(y(i,1) == 1.0){
        sumTrtleft = sumTrtleft + y(i,0)/y(i,2);
        sumTrtWtleft = sumTrtWtleft + 1/y(i,2);
      }
      else{
        sumUntrtleft = sumUntrtleft + y(i,0)/(1-y(i,2));
        sumUntrtWtleft = sumUntrtWtleft + 1/(1-y(i,2));
      }
    }
    
    if(sumTrtWtleft != 0 && sumUntrtWtleft !=0 ){
      wmeanleft =  sumTrtleft/sumTrtWtleft - sumUntrtleft/sumUntrtWtleft;
      for(int i=0;i<=j;i++) {
        rss = rss + (y(i,3) - wmeanleft)*(y(i,3) - wmeanleft);
      }
    }
    else{
      goodness[j] = 0;
      continue;
    }
    // right child
    for(int i=j+1;i<n;i++){
      if(y(i,1) == 1){
        sumTrtright = sumTrtright + y(i,0)/y(i,2);
        sumTrtWtright = sumTrtWtright + 1/y(i,2);
      }
      else{
        sumUntrtright = sumUntrtright + y(i,0)/(1-y(i,2));
        sumUntrtWtright = sumUntrtWtright + 1/(1-y(i,2));
      }
    }
    if(sumTrtWtright != 0 && sumUntrtWtright !=0 ){
      wmeanright =  -sumTrtright/sumTrtWtright + sumUntrtright/sumUntrtWtright;
      for(int i=j+1;i<n;i++){
        rss = rss + (y(i,3) + wmeanright)*(y(i,3) + wmeanright);
      }
      goodness[j] =  1/rss ;
      direction[j] = wmeanleft>0 ? 1:-1;
    }
    else{
      goodness[j] = 0;
    }
  }
  for(int i=0;i<n-1;i++){
    out[i] = goodness[i];
  }
  for(int i=n-1;i<2*(n-1);i++){
    out[i] = direction[i-n+1];
  }
  return out;
  
}