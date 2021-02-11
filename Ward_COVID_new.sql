/*We extract individuals admitted to hospital  that have tested positive for COVID-19 within 2 days of their hospital admission. For each patient, we track
their admission and discharge times, as well as any critical care episodes they may have had.*/ 

select distinct
	NHSNumber = COALESCE(HospitalEncounterServer.NHSNumber, CriticalCareServer.NHSNumber)
	,HospitalAdmissionTime = HospitalEncounterServer.AdmissionTime
	,StartTimeCriticalCare = CriticalCareServer.StartTime
	,DischargeTimeCriticalCare = CriticalCareServer.DischargeTime
	,HospitalDischargeTime = HospitalEncounterServer.DischargeTime 
	,HospitalEncounterServer.DateOfDeath
	,CriticalCareServer.BasicRespiratorySupportDays
	,CriticalCareServer.AdvancedRespiratorySupportDays
	,HospitalEncounterServer.DateOfBirth
	,Sex = Sex.NationalValue 
	
From
	HospitalEncounterServer

LEFT JOIN HospitalEncounterGlobalServer
ON HospitalEncounterGlobalServer.Recno = HospitalEncounterServer.Recno
	
LEFT JOIN CriticalCareServer
ON APCEncounterSourceUniqueID = HospitalEncounterServer.SourceUniqueID


inner join CustomDataTable.COVID19  /* Bespoke table created for UoM/Manchester Foundation Trust access - replace with corresponding table displaying COVID test information */
ON COVID19.NHSNumber = HospitalEncounterServer.NHSNumber

WHERE  (Positive = 1)
AND ((DATEDIFF(day, HospitalEncounterGlobalServer.AdmissionTime, COVID19.SpecimenCollectionDateTime ) between 0 and 2) OR (DATEDIFF(day, COVID19.SpecimenCollectionDateTime, HospitalEncounterGlobalServer.AdmissionTime) between 0 and 2))
And PatientCategoryCode = 'NE'
AND HospitalEncounterGlobalServer.AdmissionDate < '2020-05-17'
AND (NOT (BasicRespiratorySupportDays = 0 AND AdvancedRespiratorySupportDays = 0) OR (AdvancedRespiratorySupportDays is null and BasicRespiratorySupportDays is null))
ORDER BY NHSNumber asc, CriticalCareServer.StartTime asc; /* Ordering matters for python code - multiple critical care episodes should be listed in chronological order */
