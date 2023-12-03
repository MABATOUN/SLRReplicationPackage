# SLR_Replication_Package
The file SLR-ReplicationPackage.xlsx contains the results of the papers' classification and the qualitative analysis of StackOverflow topics.
The csv files were obtained using the following SQL queries in StackExchange Data Explorer database:

## Query2Results.csv:
SELECT p.Id, p.Title, p.Tags, p.Score, p.CreationDate, p.ClosedDate, p.ViewCount, p.AnswerCount, p.CommentCount, u.DisplayName, u.Id 
FROM Posts p INNER JOIN Users u ON p.OwnerUserId = u.Id 
WHERE p.Tags LIKE '%logging%'AND p.CreationDate >= '2017-01-01'
ORDER BY p.CreationDate DESC

## Query3Results.csv:
SELECT p.Id,  
       SUM(CASE WHEN v.VoteTypeId = 3 THEN 1 ELSE 0 END) AS downvotes, 
       SUM(CASE WHEN v.VoteTypeId = 2 THEN 1 ELSE 0 END) AS upvotes
FROM Posts p
LEFT JOIN Votes v ON p.Id = v.PostId
WHERE p.Tags LIKE '%logging%'AND p.CreationDate >= '2017-01-01'
GROUP BY p.id;

## Query4Results.csv:
SELECT 
q.id as q_id, q.OwnerDisplayName as q_author, q.OwnerUserId as q_oid, 
q.CreationDate as q_creationDate, q.ClosedDate as q_ClosedDate, 
q.AcceptedAnswerId as q_aaId, q.AnswerCount as q_answerCount, 
q.Tags, q.viewcount as q_viewCount,
answers.CreationDate as aa_creationDate, answers.id as aa_id, 
answers.score as aa_score, answers.OwnerDisplayName as aa_author,
answers.OwnerUserId as aa_oid
FROM 
Posts answers INNER JOIN Posts q ON answers.parentid = q.id
WHERE (answers.id = q.AcceptedAnswerId) AND (q.Tags LIKE ‘%logging%’) AND (q.CreationDate >= '2017-01-01')
ORDER BY q.CreationDate DESC
