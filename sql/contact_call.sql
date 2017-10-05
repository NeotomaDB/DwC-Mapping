SELECT cnt.ContactName FROM
NDB.Datasets AS ds INNER JOIN
NDB.DatasetPIs AS dpi ON dpi.DatasetID = ds.DatasetID LEFT OUTER JOIN
NDB.Contacts AS cnt ON cnt.ContactID = dpi.ContactID
WHERE ds.DatasetID = ?