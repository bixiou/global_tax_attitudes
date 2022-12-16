// Code to randomly generate conjoint profiles in a Qualtrics survey

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
                    

var featurearray = {"Economic issues" : ["£150 billion to upgrade schools, hospitals, care homes and council houses","Real Living Wage of £11 per hour for all workers aged 16 and over","Reduce the average full-time weekly working hours to 32","Re-establish neighbourhood policing and recruit 2,000 more frontline officers","-"],"Societal issues" : ["Strict enforcement of immigration and border legislation","Legalization of cannabis","-"],"Climate policy" : ["Ban of most polluting vehicles in city centers","Insulation plan","Ban the sale of new combustion-engine cars by 2030","-"],"Tax system" : ["National redistribution scheme","Wealth tax","-"],"Foreign policy" : ["Global climate scheme","Global tax on millionaires","Global democratic assembly on climate change","Doubling foreign aid","-"]};

var restrictionarray = [[["Economic issues","£150 billion to upgrade schools, hospitals, care homes and council houses"],["Tax system","National redistribution scheme"]],[["Economic issues","£150 billion to upgrade schools, hospitals, care homes and council houses"],["Tax system","-"]],[["Economic issues","Re-establish neighbourhood policing and recruit 2,000 more frontline officers"],["Tax system","National redistribution scheme"]],[["Economic issues","Re-establish neighbourhood policing and recruit 2,000 more frontline officers"],["Tax system","-"]],[["Climate policy","Insulation plan"],["Tax system","National redistribution scheme"]],[["Climate policy","Insulation plan"],["Tax system","-"]],[["Foreign policy","Doubling foreign aid"],["Tax system","National redistribution scheme"]],[["Foreign policy","Doubling foreign aid"],["Tax system","-"]],[["Foreign policy","Global tax on millionaires"],["Tax system","Wealth tax"]]];

var probabilityarray = {"Economic issues" : [0.2,0.2,0.2,0.2,0.2],"Societal issues" : [0.3333333333333333,0.3333333333333333,0.3333333333333333],"Climate policy" : [0.25,0.25,0.25,0.25],"Tax system" : [0.25,0.5,0.25],"Foreign policy" : [0.42857142857142855,0.14285714285714285,0.14285714285714285,0.14285714285714285,0.14285714285714285]};

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



