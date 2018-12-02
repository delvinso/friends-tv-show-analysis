# README

Analysis of the Friends series by parsing transcripts of all 236 episodes.

This repository is a complement to my post titled (The One With All the Analyses - Friends) over on my website. For more detail and insight, please see the post.

The raw transcripts can be found [here](https://fangj.github.io/friends/).

### 1. Parsing the raw transcripts.

`01_friends_parse_html.R`

* reads in and parses transcripts for all 236 episodes of the Friends series, creating a .csv file with:
	* episode id
	* episode title
	* ‘person’ speaking (not always a person, could be a scene, a title, author, etc)
	* a line
	* type of ‘line’ based on the beginning of the line

Please go through the script for more detail.

```R
# A tibble: 69,535 x 7
   id    title                                                                 person  line   type  season scene2
   <chr> <chr>                                                                 <chr>   <chr>  <chr> <chr>   <int>
 1 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) writte… marta… writ… 01          0
 2 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) [scene  centr… scene 01          1
 3 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) monica  there… pers… 01          1
 4 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) joey    c'mon… pers… 01          1
 5 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) chandl… all r… pers… 01          1
 6 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) phoebe  wait,… pers… 01          1
 7 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) (they … NA     acti… 01          1
 8 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) phoebe  just,… pers… 01          1
 9 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) monica  okay,… pers… 01          1
10 0101  The One Where Monica Gets a New Roomate (The Pilot-The Uncut Version) chandl… sound… pers… 01          1
# ... with 69,525 more rows
```

### Challenges
Although not **all** issues were reported (because I only decided to write about this after putting in a day's work of wrangling), here are some of the major ones encountered.

**Problem:** Using read_html() required many different variations of the initial parsing due to the different formats across  episodes (some required splitting on new lines, or carriage lines, retrieving text from certain nodes (<p></p> tags) and some required no formatting at all).
**Solution:** htmlTreeParse allowed for formatting based on xpath, which simplified everything.

**Problem:** (not *really* a problem, more like standard practice but..) formatting of lines
**Solution:** trimming white space, unicode characters, trailing new lines, carriage returns, cast to lower case.

**Problem:** Season 2 formatting deviated greatly from all other seasons.
	1. Some episodes lack scenes, namely ep. 2 and 3.
	2. Episodes 7, 8 and 10 have the main cast names abbreviated to 4 letters.
	3. The lines are separated by <br> tags above the line, and on the line as opposed to <p></p> on the line.
**Solution:**
	1. Not much can be done about the scenes except watching them myself (although the episode could have taken in one place).
	2. Create a dataframe and replace abbreviated names with full names of cast members.
	3. Use and if statement, if initial parsing fails resort to alternative method (guaranteed because only season 2 deviates from the standard <p></p> formatting.

**Problem:** Season 9, episodes 11 and 15 were not read properly due to formatting.
**Solution:**Went in and manually appended <p></p> to each line?

To determine the number of 'scenes’, characters appeared in a scene variable was created which is a rolling count of the number of scene.  The lines between one scene and the one before it are also captured by this variable, allowing one to determine character appearances across scenes.
	* This hinges on 'scenes' being captured correctly using regex and of course, will fluctuate based on the formatting. and have some error.
	* Captured using `[sc)|(\\((A|a)t)|(\\[(A|a)t)(.*)`
	* Check: count number of scenes across episodes, manually check the lower range?

### 2. Analyses

`02_friends_analysis.R`

The code used to generate the visualizations found in `/output` and on my post can be found in here.

There is no inherent flow to the script,  I simply answered the questions outlined in my post.  Weaved in between the questions are bits of EDA.

### 3. Output

All visualizations in my original post can be found in `output/`.


Please feel free to use any of the images, so long as you give credit where it is due!
