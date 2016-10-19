## bunch of helper functions to call procs to insert and retrieve rows from "data_science" db


#install.packages("RMySQL")
library("RMySQL", lib.loc="~/R/win-library/3.3")
library(stringr)



getDBConn <- function()
{
    db <- dbDriver("MySQL")
    dbConn <-dbConnect(db, user='root',password='noPassword',dbname='data_science');
    return(dbConn)
}


#if catName is "", then return all categories and ids
getCategorys <-function(catName)
{
    con <- getDBConn()
    cats <- dbGetQuery(con, str_c("CALL sp_get_category('", catName, "')"))
    dbDisconnect(con)
    return(cats)
}


#if skillName is "", then return all skills and ids
getSkills <-function(skill)
{
    con <- getDBConn()
    if(is.numeric(skill))
    {
        skills <- dbGetQuery(con, str_c("CALL sp_get_skillByID('", skill, "')"))
    }
    else
    {
        skills <- dbGetQuery(con, str_c("CALL sp_get_skill('", skill, "')"))
    }
    dbDisconnect(con)
    return(skills)
}


insCategoryDB <-function(catg)
{
    con <- getDBConn()
    dbGetQuery(con, str_c("CALL sp_insert_category ('", catg, "');"))
    dbDisconnect(con)
}

insSkillDB <- function(skill, catg)
{
    con <- getDBConn()
    dbGetQuery(con, str_c("CALL sp_insert_skill ('", skill, "', '", catg, "');"))
    dbDisconnect(con)
    return()
}


insTransactionByNameDB <- function(skillName)
{
    
    con <- getDBConn()
    if(as.character(getSkills(skillName)[3]) != toupper(skillName))
    {
        return(FALSE)
    }
    else
    {
        dbGetQuery(con, str_c("CALL sp_insert_transaction_by_name ('", skillName, "');"))   
        dbDisconnect(con)
        return(TRUE)
    }
}

insTransactionByIdDB <- function(skillID)
{
    con <- getDBConn()
    if(as.character(getSkills(skillID)[1]) != skillID)  ##can't insert a skill, that doesn't yet exist, don't know catg
    {
        return(FALSE)
    }
    dbGetQuery(con, str_c("CALL sp_insert_transaction_by_id (", skillID, ");"))   
    dbDisconnect(con)
    return(TRUE)
}

#this inserts a transaction with skill and catg, 
#NOTE, skill and catg do not need to yet exist, they will be inserted as needed
insTransactionByNameCatDB <- function(skill, catg)
{
    
    con <- getDBConn()
    if(as.character(toupper(getCategorys(catg)[2])) != toupper(catg))
    {
        insCategoryDB(catg)
    }
    if(as.character(toupper(getSkills(skill)[3])) != toupper(skill))
    {
        insSkillDB(skill, catg)
    }
    insTransactionByNameDB(skill)
    
}
