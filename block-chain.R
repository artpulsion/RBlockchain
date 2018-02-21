library(digest)
library(tidyverse)


### Function that creates a hashed "block"
hash_block <- function(block){
  block$new_hash <- digest(c(block$index,
                             block$timestamp,
                             block$data,
                             block$previous_hash), "sha256")
  return(block)
}


### Simple Proof of Work Algorithm
proof_of_work <- function(last_proof){
  proof <- last_proof + 1
  
  # Increment the proof number until a number is found that is divisable by 99 and by the proof of the previous block
  while (!(proof %% 99 == 0 & proof %% last_proof == 0 )){
    proof <- proof + 1
  }
  
  return(proof)
}


#A function that takes the previous block and normally some data (in our case the data is a string indicating which block in the chain it is)
gen_new_block <- function(previous_block){
  
  #Proof-of-Work
  new_proof <- proof_of_work(previous_block$proof)
  
  #Create new Block
  new_block <- list(index = previous_block$index + 1,
                    timestamp = Sys.time(),
                    data = paste0("this is block ", previous_block$index +1),
                    previous_hash = previous_block$new_hash,
                    proof = new_proof)
  
  #Hash the new Block
  new_block_hashed <- hash_block(new_block)
  
  return(new_block_hashed)
}



# Define Genesis Block (index 1 and arbitrary previous hash)
block_genesis <-  list(index = 1,
                       timestamp = Sys.time(),
                       data = "Genesis Block",
                       previous_hash = "0",
                       proof = 1)



time_elapsed <- c()

blockchain <- list(block_genesis)
previous_block <- blockchain[[1]]

# How many blocks should we add to the chain after the genesis block
num_of_blocks_to_add <- 19

# Add blocks to the chain
for (i in 1: num_of_blocks_to_add){
  
  # execution time for each new block 
  start_time <- Sys.time()
  
  block_to_add <- gen_new_block(previous_block) 
  blockchain[i+1] <- list(block_to_add)
  previous_block <- block_to_add
  
  print(cat(paste0("Block ", block_to_add$index, " has been added", "\n",
                   "\t", "Proof: ", block_to_add$proof, "\n",
                   "\t", "Hash: ", block_to_add$new_hash)))
  
  
  end_time <- Sys.time()
  time_elapsed <- c(time_elapsed, end_time - start_time)
  
  }


exec_data = cbind(time_elapsed, rep(1:length(time_elapsed))) %>% data.frame
colnames(exec_data) = c('time_elapsed', 'index_block')

ggplot2:::ggplot(exec_data, aes(index_block, time_elapsed )) +
  geom_line() +
  labs(title = "Time it takes to mine a new block") +
  ylab("System time elapsed (in sec.)") +
  xlab("Block index")

