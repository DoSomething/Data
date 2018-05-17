SELECT DISTINCT s.id
FROM rogue.signups s
INNER JOIN  
	(SELECT g.id
	FROM rogue.signups g
	WHERE g.why_participated = 'why_participated_ghost_test') ghost ON s.id = ghost.id

