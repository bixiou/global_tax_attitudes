Qualtrics.SurveyEngine.addOnload(function()
{// Code to randomly generate conjoint profiles in a Qualtrics survey
// F
	var country = "${q://QID130/ChoiceGroup/SelectedChoices}"
	if (country == "France" || country == "La France" || country == "Frankreich" || country == "Francia") {
		var featurearray = {
            "FR_econ_issues" : ["FR_econ1","FR_econ2","FR_econ3","FR_econ4","-"],
            "FR_society_issues" : ["FR_soc1","FR_soc2","-"],
            "FR_climate_pol" : ["FR_climate1","FR_climate2","FR_climate3","-"],
            "FR_tax_system" : ["FR_tax1","FR_tax2","-"],
            "FR_foreign_policy" : ["FR_foreign1","FR_foreign2","FR_foreign3","FR_foreign4","-"]};

			// var restrictionarray = [
			// 	[["FR_econ_issues","FR_econ1"],["FR_tax_system","FR_tax1"]],
			// 	[["FR_econ_issues","FR_econ1"],["FR_tax_system","-"]],
			// 	[["FR_econ_issues","FR_econ4"],["FR_tax_system","FR_tax1"]],
			// 	[["FR_econ_issues","FR_econ4"],["FR_tax_system","-"]],
			// 	[["FR_climate_pol","FR_climate2"],["FR_tax_system","FR_tax1"]],
			// 	[["FR_climate_pol","FR_climate2"],["FR_tax_system","-"]],
			// 	[["FR_foreign_policy","FR_foreign4"],["FR_tax_system","FR_tax1"]],
			// 	[["FR_foreign_policy","FR_foreign4"],["FR_tax_system","-"]],
			// 	[["FR_foreign_policy","FR_foreign2"],["FR_tax_system","FR_tax2"]]];
		
		var probabilityarray = {
            "FR_econ_issues" : [0.2,0.2,0.2,0.2,0.2], // Don't forget to adjust in all countries and .dat if you edit the probas
            "FR_society_issues" : [0.3333333333333333,0.3333333333333333,0.3333333333333333],
            "FR_climate_pol" : [0.25,0.25,0.25,0.25],
            "FR_tax_system" : [0.35,0.4,0.25],
            "FR_foreign_policy" : [0.42857142857142855,0.14285714285714285,0.14285714285714285,0.14285714285714285,0.14285714285714285]};					
	} else if (country == "Deutschland" || country == "Germany" || country == "Allemagne" || country == "Alemania") {
		var featurearray = {"DE_econ_issues" : ["DE_econ1","DE_econ2","DE_econ3","DE_econ4","-"],"DE_society_issues" : ["DE_soc1","DE_soc2","-"],"DE_climate_pol" : ["DE_climate1","DE_climate2","DE_climate3","-"],"DE_tax_system" : ["DE_tax1","DE_tax2","-"],"DE_foreign_policy" : ["DE_foreign1","DE_foreign2","DE_foreign3","DE_foreign4","-"]};

		// var restrictionarray = [[["DE_econ_issues","DE_econ1"],["DE_tax_system","DE_tax1"]],[["DE_econ_issues","DE_econ1"],["DE_tax_system","-"]],[["DE_econ_issues","DE_econ4"],["DE_tax_system","DE_tax1"]],[["DE_econ_issues","DE_econ4"],["DE_tax_system","-"]],[["DE_climate_pol","DE_climate2"],["DE_tax_system","DE_tax1"]],[["DE_climate_pol","DE_climate2"],["DE_tax_system","-"]],[["DE_foreign_policy","DE_foreign4"],["DE_tax_system","DE_tax1"]],[["DE_foreign_policy","DE_foreign4"],["DE_tax_system","-"]],[["DE_foreign_policy","DE_foreign2"],["DE_tax_system","DE_tax2"]]];
		
		var probabilityarray = {"DE_econ_issues" : [0.2,0.2,0.2,0.2,0.2],"DE_society_issues" : [0.3333333333333333,0.3333333333333333,0.3333333333333333],"DE_climate_pol" : [0.25,0.25,0.25,0.25],"DE_tax_system" : [0.35,0.4,0.25],"DE_foreign_policy" : [0.42857142857142855,0.14285714285714285,0.14285714285714285,0.14285714285714285,0.14285714285714285]};								 
	} else if (country == "Spain" || country == "España" || country == "Espagne" || country == "Spanien")  {
		var featurearray = {"ES_econ_issues" : ["ES_econ1","ES_econ2","ES_econ3","ES_econ4","-"],"ES_society_issues" : ["ES_soc1","ES_soc2","-"],"ES_climate_pol" : ["ES_climate1","ES_climate2","ES_climate3","-"],"ES_tax_system" : ["ES_tax1","ES_tax2","-"],"ES_foreign_policy" : ["ES_foreign1","ES_foreign2","ES_foreign3","ES_foreign4","-"]};

		// var restrictionarray = [[["ES_econ_issues","ES_econ1"],["ES_tax_system","ES_tax1"]],[["ES_econ_issues","ES_econ1"],["ES_tax_system","-"]],[["ES_econ_issues","ES_econ4"],["ES_tax_system","ES_tax1"]],[["ES_econ_issues","ES_econ4"],["ES_tax_system","-"]],[["ES_climate_pol","ES_climate2"],["ES_tax_system","ES_tax1"]],[["ES_climate_pol","ES_climate2"],["ES_tax_system","-"]],[["ES_foreign_policy","ES_foreign4"],["ES_tax_system","ES_tax1"]],[["ES_foreign_policy","ES_foreign4"],["ES_tax_system","-"]],[["ES_foreign_policy","ES_foreign2"],["ES_tax_system","ES_tax2"]]];
		
		var probabilityarray = {"ES_econ_issues" : [0.2,0.2,0.2,0.2,0.2],"ES_society_issues" : [0.3333333333333333,0.3333333333333333,0.3333333333333333],"ES_climate_pol" : [0.25,0.25,0.25,0.25],"ES_tax_system" : [0.35,0.4,0.25],"ES_foreign_policy" : [0.42857142857142855,0.14285714285714285,0.14285714285714285,0.14285714285714285,0.14285714285714285]};								
	} else if (country == "United Kingdom" || country == "The United Kingdom" || country == "Reino Unido" || country == "Royaume-Uni" || country == "Vereinigtes Königreich") {         
		var featurearray = {"UK_econ_issues" : ["UK_econ1","UK_econ2","UK_econ3","UK_econ4","-"],"UK_society_issues" : ["UK_soc1","UK_soc2","-"],"UK_climate_pol" : ["UK_climate1","UK_climate2","UK_climate3","-"],"UK_tax_system" : ["UK_tax1","UK_tax2","-"],"UK_foreign_policy" : ["UK_foreign1","UK_foreign2","UK_foreign3","UK_foreign4","-"]};

		// var restrictionarray = [[["UK_econ_issues","UK_econ1"],["UK_tax_system","UK_tax1"]],[["UK_econ_issues","UK_econ1"],["UK_tax_system","-"]],[["UK_econ_issues","UK_econ4"],["UK_tax_system","UK_tax1"]],[["UK_econ_issues","UK_econ4"],["UK_tax_system","-"]],[["UK_climate_pol","UK_climate2"],["UK_tax_system","UK_tax1"]],[["UK_climate_pol","UK_climate2"],["UK_tax_system","-"]],[["UK_foreign_policy","UK_foreign4"],["UK_tax_system","UK_tax1"]],[["UK_foreign_policy","UK_foreign4"],["UK_tax_system","-"]],[["UK_foreign_policy","UK_foreign2"],["UK_tax_system","UK_tax2"]]];
		
		var probabilityarray = {"UK_econ_issues" : [0.2,0.2,0.2,0.2,0.2],"UK_society_issues" : [0.3333333333333333,0.3333333333333333,0.3333333333333333],"UK_climate_pol" : [0.25,0.25,0.25,0.25],"UK_tax_system" : [0.35,0.4,0.25],"UK_foreign_policy" : [0.42857142857142855,0.14285714285714285,0.14285714285714285,0.14285714285714285,0.14285714285714285]};				
	}

	var restrictionarray = [];
// Terminology clarification: 
// Task = Set of choices presented to respondent in a single screen (i.e. pair of candidates)
// Profile = Single list of attributes in a given task (i.e. candidate)
// Attribute = Category characterized by a set of levels (i.e. education level)
// Level = Value that an attribute can take in a particular choice task (i.e. "no formal education")

// Attributes and Levels stored in a 2-dimensional Array 

/* Randomize array in-place using Durstenfeld shuffle algorithm */
function shuffleArray(array) {
    for (var i = array.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    return(array);
}

// Function to generate weighted random numbers
function weighted_randomize(prob_array, at_key)
{
	var prob_list = prob_array[at_key];
	
	// Create an array containing cutpoints for randomization
	var cumul_prob = new Array(prob_list.length);
	var cumulative = 0.0;
	for (var i=0;  i < prob_list.length; i++){
		cumul_prob[i] = cumulative;
		cumulative = cumulative + parseFloat(prob_list[i]);
	}

	// Generate a uniform random floating point value between 0.0 and 1.0
	var unif_rand = Math.random();

	// Figure out which integer should be returned
	var outInt = 0;
	for (var k = 0; k < cumul_prob.length; k++){
		if (cumul_prob[k] <= unif_rand){
			outInt = k + 1;
		}
	}

	return(outInt);

}
                    
// Indicator for whether weighted randomization should be enabled or not
var weighted = 1;

// K = Number of tasks displayed to the respondent
var K = 1;

// N = Number of profiles displayed in each task
var N = 2;

// num_attributes = Number of Attributes in the Array
var num_attributes = featurearray.length;

// Should duplicate profiles be rejected?
var noDuplicateProfiles = true;

var attrconstraintarray = [];

// Re-randomize the featurearray

// Place the $featurearray keys into a new array
var featureArrayKeys = Object.keys(featurearray);

// If order randomization constraints exist, drop all of the non-free attributes
if (attrconstraintarray.length != 0){
	for (const constraints of attrconstraintarray){
		if (constraints.length > 1){
			for (var p = 1; p < constraints.length; p++){
				if (featureArrayKeys.includes(constraints[p])){
					var remkey = featureArrayKeys.indexOf(constraints[p]);
                    featureArrayKeys.splice(remkey, 1);
				}
			}
		}
	}
} 

// Re-randomize the featurearray keys
featureArrayKeys = shuffleArray(featureArrayKeys);

// Re-insert the non-free attributes constrained by $attrconstraintarray
if (attrconstraintarray.length != 0){
	for (const constraints of attrconstraintarray){
		if (constraints.length > 1){
			var insertloc = constraints[0];
			if (featureArrayKeys.includes(insertloc)){
				var insert_block = [];
				for (var p = 1; p < constraints.length; p++){
          insert_block.push(constraints[p]);
				}
				var begin_index = featureArrayKeys.indexOf(insertloc);
				featureArrayKeys.splice(begin_index+1, 0, ...insert_block);
			}
		}
	}
}


// Re-generate the new $featurearray - label it $featureArrayNew
var featureArrayNew = {};
for (var h = 0; h < featureArrayKeys.length; h++){
    featureArrayNew[featureArrayKeys[h]] = featurearray[featureArrayKeys[h]];        
}


// Initialize the array returned to the user
// Naming Convention
// Level Name: F-[task number]-[profile number]-[attribute number]
// Attribute Name: F-[task number]-[attribute number]
// Example: F-1-3-2, Returns the level corresponding to Task 1, Profile 3, Attribute 2 
// F-3-3, Returns the attribute name corresponding to Task 3, Attribute 3

var returnarray = {};

// For each task $p
for(var p = 1; p <= K; p++){

	// For each profile $i
	for(var i = 1; i <= N; i++){

		// Repeat until non-restricted profile generated
		var complete = false;

		while (complete == false){

			// Create a count for $attributes to be incremented in the next loop
			var attr = 0;
			
			// Create a dictionary to hold profile's attributes
			var profile_dict = {};

			// For each attribute $attribute and level array $levels in task $p
			for(var q = 0; q < featureArrayKeys.length; q++){
				// Get Attribute name
				var attr_name = featureArrayKeys[q];
					
				// Increment attribute count
				attr = attr + 1;
	
				// Create key for attribute name
				var attr_key = "F-" + p + "-" + attr;
	
                // Store attribute name in returnarray
                returnarray[attr_key] = attr_name;

				// Get length of levels array
				var num_levels = featureArrayNew[attr_name].length;

				// Randomly select one of the level indices
				if (weighted == 1){
					var level_index = weighted_randomize(probabilityarray, attr_name) - 1;

				}else{
					var level_index = Math.floor(Math.random() * num_levels);
				}	

				// Pull out the selected level
				var chosen_level = featureArrayNew[attr_name][level_index];
				
				// Store selected level in profileDict
				profile_dict[attr_name] = chosen_level;
	
				// Create key for level in $returnarray
				var level_key = "F-" + p + "-" + i + "-" + attr;
	
				// Store selected level in $returnarray
				returnarray[level_key] = chosen_level;

			}

            var clear = true;
            
            // Cycle through restrictions to confirm/reject profile
            if (restrictionarray.length != 0){
                for (var v = 0; v < restrictionarray.length; v++){
                    var falsevar = 1;
                    for (var mp = 0; mp < restrictionarray[v].length; mp++){
                        if (profile_dict[restrictionarray[v][mp][0]] == restrictionarray[v][mp][1]){
                            falsevar = falsevar*1;
                        }else{
                            falsevar = falsevar*0;
                        }							
                    }
                    if (falsevar == 1){
                        clear = false;
                    }
                }
            }
                            
            // If we're throwing out duplicates
            if (noDuplicateProfiles == true){
                // Cycle through all previous profiles to confirm no identical profiles
                if (i > 1){    
                    // For each previous profile
                    for(var z = 1; z < i; z++){
    					
                        // Start by assuming it's the same
                        var identical = true;
    					
                        // Create a count for $attributes to be incremented in the next loop
                        var attrTemp = 0;
    					
                        // For each attribute $attribute and level array $levels in task $p
                        for(var qz = 0; qz < featureArrayKeys.length; qz++){
    						
                            // Increment attribute count
                            attrTemp = attrTemp + 1;
    
                            // Create keys 
                            var level_key_profile = "F-" + p + "-" + i + "-" + attrTemp;
                            var level_key_check = "F-" + p + "-" + z + "-" + attrTemp;
    						
                            // If attributes are different, declare not identical
                            if (returnarray[level_key_profile] != returnarray[level_key_check]){
                                identical = false;
                            }
                        }
                        // If we detect an identical profile, reject
                        if (identical == true){
                            clear = false;
                        }
                    }                
                }
            }
            complete = clear;
        }
    }
}
                            
// Write returnarray to Qualtrics

var returnarrayKeys = Object.keys(returnarray);

for (var pr = 0; pr < returnarrayKeys.length; pr++){
       Qualtrics.SurveyEngine.setEmbeddedData(returnarrayKeys[pr], returnarray[returnarrayKeys[pr]]); 
}




});

Qualtrics.SurveyEngine.addOnReady(function()
{
	/*Place your JavaScript here to run when the page is fully displayed*/

});

Qualtrics.SurveyEngine.addOnUnload(function()
{
	/*Place your JavaScript here to run when the page is unloaded*/

});
