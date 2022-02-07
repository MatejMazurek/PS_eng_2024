# ......................................................................................
# ..................Exercise 1 - Brief introduction to R, Combinatorics.................
# ......................................................................................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Combinatorics ####


# * r-permutations ####
#  
# P(n, k) - r-permutations, the first argument will be the total number of entities, the
# second argument the size of the selection
#  


# the function is created by the fucntion command, it is an object whose name is given by a variable
# to which I will assign this object
r_permutation = function(n,k) # here I enter the number of parameters and their names
{ # the whole body of the function is enclosed in parentheses {...}
    numerator = factorial(n)  # the factorial in the original R exists so we will use it
    denominator = factorial(n-k)
    return(numerator/denominator)    # what the function returns is given in the return(...) statement
}

# P*(n, k) - r-permutations with repetition
#  


r_permutation_repetition = function(n,k)
{
  return(n^k)
}

# * Permutation ####
#  
# P(n)=P(n, n) - permutation
#  


permutation = function(n)
{
  return(r_permutation(n,n))
}

# P*(n1, n2, n3,...., nk) - permutation with repetition, input will be a vector with
# individual numbers of unique entities
#  


permutation_repetition = function(vec_n) # vec_n is the vector of values eg: vec_n=c(2,2,2,4,3)
{
    n = sum(vec_n) # we calculate how many values we have in total
    res_temp=factorial(n) # their factorial=value in the numerator
    # a simple loop starts with the for statement, then the iterator name follows in parentheses
    # and what list will be taken as iterable object
    for(count in vec_n) # count is an iterator and will gradually take values from the vector vec_n
    {
        # we gradually divide by the factorial of each number of unique entities
        res_temp=res_temp/factorial(count) 
    }
    return(res_temp)
}

# * Combination ####
#  
# C(n, k) - combination
#  


combinations = function(n,k)
{
  return(choose(n,k)) # the function for combination already exists in R and is called choose
}

# C*(n, k) - combination with repetition
#  


combinations_repetition = function(n,k)
{
  return(choose(n+k-1,k)) # we use a known formula
}

#  Exercise tasks ####
#  
# * Example 1. ####
#  
# There are three types of locks available in the store. To open the first lock, it is
# necessary to press four of the ten buttons marked with numbers 0 to 9.(The order does
# not matter - the buttons remain pressed.) The second lock will open if we press six of
# the ten buttons. To open the third lock, it is necessary to set the correct
# combination on the four discs. Which of these locks best protects against thieves?
#  


z1=combinations(10,4)
z2=combinations(10,6)
z3=r_permutation_repetition(10,4)
paste("count of possible passwords: ",z1,",",z2,",",z3)
paste("probability od random opening: ",1/z1,",",1/z2,",",1/z3)

# * Example 2. ####
#  
# The store offers two types of briefcase locking. The first briefcase is locked with a
# six-digit cipher. The second case is locked with two locks that open at the same time.
# The cipher of each of them consists of three digits. Determine for each briefcase the
# probability of opening by a thief on the first attempt. Which type of lock is safer?
#  


z1=r_permutation_repetition(10,6);
z2=r_permutation_repetition(10,3)*r_permutation_repetition(10,3);
z2_v2=r_permutation_repetition(10,3)+r_permutation_repetition(10,3);
paste("count combinations: ",z1,",",z2,",second variant - ",z2_v2)

# * Example 3. ####
#  
# There are 40 balls in the urn - 2 red and 38 white. We randomly pull 2 balls out of
# the urn. How likely are they both to be red?
#  


all_combinations=combinations(40,2);
combinations_valid=combinations(2,2);
prob=combinations_valid/all_combinations;
paste("probability is: ",prob)

# * Example 4. ####
#  
# The student had to prepare answers to 40 questions for the exam. He could not answer
# the two questions the examiner asked him, so he said, “I'm unlucky! These are the only
# two questions I can't answer. ”How likely is he to tell the truth?
#  




# * Example 5. ####
#  
# A student passes a chemistry test if he underlines the only two aldehydes on the list
# of 40 chemical compounds. What is the probability that a student who underlines the
# compounds at random will pass the test?
#  




# * Example 6. ####
#  
# A group of 40 tourists returned from abroad, including 2 smugglers. At the border,
# customs officer called for a personal search of two passengers and it turned out that
# both were smugglers. The remaining tourists responded: "The customs officer was really
# lucky!", "Someone reported the smugglers!",.... How to deal with these statements? Is
# there a legitimate suspicion that someone reported the smugglers?
#  




# * Example 7. ####
#  
# From the urn with three balls, two red and one white, two balls will be selected at
# the same time. The student and the teacher place a bet. If both balls are of the same
# color, the student wins. If the balls have different colors, the teacher wins. Is the
# game fair? What are the probabilities of a teacher and a student winning?
#  


# the combn function produces combinations of the specified size - the first parameter is a vector of values, the second the size of the selection
combn(c('red','red','white'),2)

# * Example 8. ####
#  
# The game described in Example 7 was not fair. What ball(red or white) do we need to
# add to the urn to make the game fair?
#  


combn(c('red','red','white','white'),2)
combn(c('red','red','white', 'red'),2)

# * Example 9. ####
#  
# You want to play "Man, don't be angry" (a board game requiring a dice), but all the
# dices are lost. How can a dice be replaced if you have playing cards(a deck of 32
# cards) and 4 different colored balls?
#  




# * Example 10. ####
#  
# You want to play Man, don't be angry, but the dice is lost. How can one replace a dice
# if he has only 3 different colored balls?
#  




# * Example 11. ####
#  
# They have a sales event in the Škoda car dealership in February. In addition to the
# standard equipment, they offer 3 items from the above-standard equipment free of
# charge. Extra equipment includes 7 items:
#  
# - cruise control, seat heating, rear airbags, xenon headlights, sunroof, gearbox lock,
# and  special durable metallic paint. 
# 
# How many options does the customer have to choose 3 items from the above-standard
# equipment?
#  


combinations(7,3)

# * Example 12. ####
#  
# During the exam, 12 students will be sitting in the 5th row. The examiner wants to
# determine himself where every student will be sitting.
#  
# - How many ways to organize those 12 students?
#  
# - Student Brahý asks to be able to sit on aisle seat and leave earlier to catch the
# train. How many options are there for deploying students if the examiner wants to meet
# Brahý's request?
#  
# - How many opportunities are there to organize the students if Pažout and Horáček are
# not allowed to sit next to each other?
#  


# a
permutation(12)
# b
1*permutation(11)+permutation(11)*1
# c
together=permutation(11)+permutation(11)
permutation(12)-together

# * Example 13. ####
#  
# How many anagrams can be created from the word STATISTICS?
#  


# 3S,3T,1A,2I,1C  
STATISTICS=c(3,3,1,2,1)
permutation_repetition(STATISTICS)

# * Example 14. ####
#  
# They got new goods in Tesco - boys' T-shirt in 6 different colors. They have at least
# 7 pieces of each color. The mother wants to buy 4 T-shirts for her son. How many
# options are there to choose from
#  
# - if they have to be all to be different?
#  
# - if they can all be the same?
#  


# a
combinations(6,4)
# b
combinations_repetition(6,4)

# * Example 15. ####
#  
# How many passwords of length 5 can we create from alphabetic characters
#  
# - if case insensitive?
#  
# - if case sensitive?
#  


# a
r_permutation_repetition(26,5)
# b
r_permutation_repetition(52,5)



#  Bonus Exercise tasks ####
#  
# * Bonus Example 1. ####
#  
# Which password is more secure?
#  
# * An eight-character password consisting of numbers only.
#  
# * A five-character password composed only of letters of the English alphabet.
#  


# password 1
h1 = r_permutation_repetition(n = 10, k = 8)
# password 2
h2 = r_permutation_repetition(n = 26, k = 5)
h1/h2

# * Bonus Example 2. ####
#  
# How long would it take to solve a business traveler's problem for n=10 cities by brute
# force if the evaluation of the length of each of the possible journeys takes 1 µs?
#  


n = 10
count = permutation(n-1)/2
time = count/1000000
time

# * Bonus Example 3. ####
#  
# How to divide the booty between 2 robbers to get both items of the same value(or as
# close as possible). I.e. can I divide N numbers into two groups so that the sum of the
# numbers in each group is the same?
#  
# **How many options would have to be tried if we solved the problem with brute force?**
#  


N = 10
L = 4
r_permutation_repetition(n = L, k = N)

# * Bonus Example 4. ####
#  
# How many anagrams of the word "AUTO" can we create? How many anagrams of the word
# "AUTOMOBILKA" can we create? How many of them start with "K"?
#  


permutation(4)
vec = c(2,1,1,2,1,1,1,1,1)
sum(vec)
permutation_repetition(vec)

vec = c(2,1,1,2,1,1,1,1)
sum(vec)
permutation_repetition(vec)

# * Bonus Example 5. ####
#  
# They have 6 types of colored cups in the shop.
#  
# - How many different ways can we buy 4 different-colored mugs?
#  
# - How many different options can we buy 5 cups(if we don't mind more of the same
# color)?
#  
# - How will the situation change if they have only 4 pieces of each(and we don't mind
# more of the same color)?
#  


combinations(6,4)
combinations_repetition(6,5)
combinations_repetition(6,5) - 6

# * Bonus Example 6. ####
#  
# There are 5 different pairs of socks in the package (the left and right socks are
# always the same).
#  
# - How many different pairs of socks can be chosen?
#  
# - How many different ways can I wear it? (i.e. it is important what sock is on which
# leg)
#  


combinations_repetition(n = 5,k = 2)
r_permutation_repetition(n=5,k=2)
combinations_repetition(n = 5,k = 2)*2 - 5

# * Bonus Example 7. ####
#  
# I have 12 weights weighing 1,2,..., 12 kg.
#  
# - How many ways can I divide them into 2 piles?
#  
# - How many ways can I divide them into 3 piles?
#  
# - How many ways can I divide them into 3 piles if they all have the same number of
# weights?
#  
# - How many ways can I divide them into 3 piles of the same number of weights, if the
# weight of none of the pile can exceed 40 kg?
#  


r_permutation_repetition(2,12)
r_permutation_repetition(3,12)
(r_permutation_repetition(3,12)-3)/permutation(3)+1
(r_permutation_repetition(3,12)-(r_permutation_repetition(2,12)-2)*3-3)/permutation(3)
combinations(12,4)*combinations(8,4)/permutation(3)
permutation(12)/(permutation(4)*permutation(4)*permutation(4)*permutation(3))
combinations(12,4)*combinations(8,4)/permutation(3)-combinations(8,4)

# * Bonus Example 8. ####
#  
# I have 20 seeds from each of the three vegetables(carrots, radishes, celery).
# Unfortunately, they are mixed up and unrecognizable from each other.
#  
# - I will plant 5 random seeds in the box. What is the probability that there will be
# at least three radishes among them?
#  
# - I'll plant 5 random seeds in the box. What is the probability that there will be
# more carrots than celery among them?
#  


(combinations(20,3)*combinations(40,2)+combinations(20,4)*combinations(40,1)+combinations(20,5))/combinations(60,5)

