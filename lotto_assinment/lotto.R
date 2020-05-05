rm(list = ls())
select_lotto_number = function(number,num_game=5){
    if(class(number) == "list"){
        for(i in 1:num_game){
            if(all(is.na(number[[i]]))) number[[i]] = sample(1:45,6)
        }
    }else{
        number = list()
        for(i in 1:num_game){
            number[[i]] = sample(1:45,6)
        }
    }
    return(number)
}

get_lotto_number = function(){
    number = sample(1:45,7)
    win = number[1:6]
    bonus = number[7]
    return(list(win=win, bonus = bonus))
}

matching = function(real_numbers,select_numbers){
    match_count = sum(as.numeric(real_numbers$win %in% select_numbers))
    cash = 0
    if(match_count==6) cash=2e9
    else if(match_count==5){
        if(real_numbers$bonus %in% select_numbers) cash=6e7  else cash=1.5e6
    }
    else if(match_count==4) cash=5e4
    else if(match_count==3) cash=5e3
    else cash=0
    return(list(cash=cash, match_count = match_count))
}

onetime_lotto = function(number_list = NA, num_game = 5){
    if(class(number_list) == "list") num_game = length(number_list)
    select_number = select_lotto_number(number_list, num_game)
    real_numbers = get_lotto_number()
    cash = vector()
    match_count = vector()
    for(i in 1:num_game){
        cash_match = matching(real_numbers,select_number[[i]])
        cash[i] = cash_match$cash
        match_count[i] = cash_match$match_count
    }
    return(list(total_reward = sum(cash),total_income=sum(cash) - num_game*1000, match_count = match_count))
}
onetime_lotto(num_game = 5)
number_list = list(c(34,6,24,12,7,8),NA,c(45,22,3,13,16,34),NA,NA)
onetime_lotto(number_list)
set.seed(12151487)
x = select_lotto_number(number_list)
y = get_lotto_number()

matching(y, c(5,18,27,25,24,12))
set.seed(12151487)
total_income = 0
each_income = vector()
for (i in 1:as.integer(365*20/7)){
    onetime = onetime_lotto(num_game = 4)
    total_income = total_income + onetime$total_income
    each_income[i] = onetime$total_income
}
cat("Total Income : ", total_income)
each_income
