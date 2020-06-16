/*We extract individuals admitted to hospital  that have tested positive for COVID-19 within 2 days of their hospital admission. For each patient, we track
their admission and discharge times, as well as any critical care episodes they may have had.*/ 

select distinct
	NHSNumber = COALESCE(Encounter.NHSNumber, Episode.NHSNumber)
	,HospitalAdmissionTime = EncounterGlobal.AdmissionTime
	,StartTimeCriticalCare = Episode.StartTime
	,DischargeTimeCriticalCare = Episode.DischargeTime
	,HospitalDischargeTime = EncounterGlobal.DischargeTime 
	,Encounter.DateOfDeath
	,Episode.BasicRespiratorySupportDays
	,Episode.AdvancedRespiratorySupportDays
	,Encounter.DateOfBirth
	,Sex = Sex.NationalValue 
	
From
	ModelAPC.Dimension.Encounter

LEFT JOIN ModelAPC.Dimension.EncounterGlobal
ON EncounterGlobal.Recno = Encounter.Recno

inner join ModelAPC.Dimension.DischargeDestination
 on	DischargeDestination.SourceValueID = Encounter.DischargeDestinationID

inner join ModelAPC.Dimension.AdmissionSource
on AdmissionSource.SourceValueID = Encounter.AdmissionSourceID

inner join ModelAPC.Dimension.Sex
on Sex.SourceValueID = Encounter.SexID
	
LEFT JOIN ModelCriticalCare.Dimension.Episode
ON APCEncounterSourceUniqueID = Encounter.SourceUniqueID


inner join SandboxUoM.dbo.COVID19  /* Bespoke table created for UoM/Manchester Foundation Trust access - replace with corresponding table displaying COVID test information */
ON COVID19.NHSNumber = Encounter.NHSNumber

WHERE  (Positive = 1)
AND ((DATEDIFF(day, EncounterGlobal.AdmissionTime, COVID19.SpecimenCollectionDateTime ) between 0 and 2) OR (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, EncounterGlobal.AdmissionTime) between 0 and 2))
And PatientCategoryCode = 'NE'
AND EncounterGlobal.AdmissionDate < '2020-05-17'
AND (NOT (BasicRespiratorySupportDays = 0 AND AdvancedRespiratorySupportDays = 0) OR (AdvancedRespiratorySupportDays is null and BasicRespiratorySupportDays is null))
ORDER BY NHSNumber asc, Episode.StartTime asc; /* Ordering matters for python code - multiple critical care episodes should be listed in chronological order */
