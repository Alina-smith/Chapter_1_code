## cascade model function
# Zooplankton can eat phytoplankton and other zooplankton samller than them, phytoplankton canno eat anything
function cascade_model_tian(C; mprod = [], minvert=[])

    ## Make a list of invertibrates and producers in mass order
    # find number of producers and invertibrates
    nprod = length(mprod)
    ninvert = length(minvert)
    # Give the invertibrates and phytoplankton names by numbering them and adding either P or zoo infront
    phyto_names = "p".* [string(i) for i in 1:nprod] #loop over 1 to nprod and for each one turn to a string and then add P infront by broadcasting P to the whole loop 
    invert_names = "zoo".* [string(i) for i in 1:ninvert] #give the zooplankton names using the same method as above
    # calculate the species richness 
    S = nprod + ninvert
    # Sort producers and invertibrates in mass order
    idx_sort_mass_total = sortperm([mprod; minvert]) # sortperm gives a vector with the id (the position of the species in the orrigional vector) but in the order of their masses, the ; in a square bracket concatenates the two vectors together

    ## Build the matrix of possible interaction
    # build a matrix of 1s
    M = ones(Bool,S, S) # bool means that the 1s == true and 0s == false which is usefull later when some parts of the matrix are turned to 0 as this says if an interaction is there or not
    
    # Select possible links: invertebrate - producers
    # make the interactions only between species smaller than them
    lower_triangle = tril(M, -1) #selects the lower triangle so upper triangle is set to 0, -1 means it starts this from 1 row from the top so that the diagnonal is not selected
    # Remove interactions between producers by removing producer rows 
    prod_idx = findall(idx_sort_mass_total .∈ [1:nprod]) # tells you which ones in idx_sort_mass_total are producers. 
    invert_idx = findall(idx_sort_mass_total .∈ [nprod+1:S]) # same as above for invertibrates
    idx = [prod_idx; invert_idx]
    possible_links = lower_triangle[setdiff(1:S, prod_idx),:] # find the values from 1:S that are not present in prod_idx and i
    # gets a list all the values from 1:S that are not present in prod_idx - gets a list of only non producers, : = all columns, so removes the rows with producers

    ## Randomly assign links 
    # calculate necessary things
    L = C * (S-1) * S # actual number of links
    num_possible_links = sum(possible_links) #number of all possible links
    p = L / num_possible_links # the probability of a link forming is equal to the denisty of links (number of actual links/number of possible links)
    # error incase actual number of links is over the possible number of links 
    if L > num_possible_links
        error("L > num of possible links, please decrease C!")
    end
    
    # Set links
    sampling_link = rand(num_possible_links) # Generate an array the length of num_possible_links containing random numbers between 0 and 1
    # set numbers in sampling_link to 0 (no interaction) that are greater than or equal to P 
    sampling_link[findall(>=(p), sampling_link)] .= 0
    # set numbers in sampling_link to 1 (interaction) that are smaller than P
    sampling_link[findall(>(0), sampling_link)] .= 1
    # Replace the possible links with the randomly assigned links
    possible_links[possible_links .== true] = sampling_link # select the possitive values (interactions) in possible_links and assign the value of sampling_links to them 

    # Make an interaction matrix using the sampling_link info
    A = [] # empty array
    n = 1 # reference number for the row of possible_links
    for i in 1:S
        if i ∈ prod_idx
            push!(A, repeat([0], S)) # if the value is in prod_idx then append an array of 0s the length of S
        else
            push!(A, possible_links[n, :]) # if the value is not present in prod_idx then append the nth row from possible links
            n += 1 # just increase the row number each time as the first 
        end
    end

    A = reduce(hcat, A)' # turn it into a matrix

    ## Reorder columns as the provived vector of producer masses and zoomasses [mprod; minvert]
    A_resorted = A[idx, idx]
    fw = Foodweb(A_resorted)
    #fw = Foodweb(A_resorted, M = [mprod; minvert], species = [phyto_names; invert_names],
    #             metabolic_class = [repeat(["producer"], nprod); repeat(["invertebrate"], ninvert)])
    fw
end

# # Test the final connectance for cascade
# fw_test = cascade_model_tian(.2; mprod = [1, 10], minvert = [5, 8, 15])
# sum(fw_test.A) / ((richness(fw_test) - 1) * richness(fw_test)) ## all good

