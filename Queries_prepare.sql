SELECT * FROM dailyactivity_merged
------

SELECT * FROM sleepday

------

SELECT * FROM weightlog_info

------

SELECT * FROM hourlySteps_merged

------
--Examining the data set by looking at the number respondents and number of activities per respondent

SELECT id, COUNT(id) FROM dailyactivity_merged
GROUP BY id
HAVING COUNT(id) <= 20
ORDER BY COUNT(id)

------

SELECT id, COUNT(id) FROM sleepday
GROUP BY id
ORDER BY COUNT(id)

------

SELECT id, COUNT(id) FROM weightlog_info
GROUP BY id
ORDER BY COUNT(id)

------
