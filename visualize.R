## ---
## This contains functions that do various bits
## of the agent based model
## ---

## prep
if( !require( ggforce ) ) { devtools::install_github( 'thomasp85/ggforce' ) }; library( ggforce )
if( !require( RColorBrewer ) ) { install.packages( 'RColorBrewer' ) }; library( RColorBrewer )
if( !require( ggnetwork ) ) { install.packages( 'ggnetwork' ) }; library( ggnetwork )
if( !require( network ) ) { install.packages( 'network' ) }; library( network )
if( !require( ggplot2 ) ) { install.packages( 'ggplot2' ) }; library( ggplot2 )
if( !require( OpenImageR ) ) { install.packages( 'OpenImageR' ) }
if( !require( reshape2 ) ) { install.packages( 'reshape2' ) }


## ---
## Initialize
## ---

## load data
res <- read.table( './out.tsv', sep = '\t' )

## add header
names( res ) <- c( 'time', 'id', 'x', 'y', 'state', 'in.a.patch' )

## load landscape
landscape <- as.matrix( read.table( './landscape.tsv', sep = '\t', header = F ) )

## get range of my arena
arena_x <- range( res$x ) + c(-1, 1)
arena_y <- range( res$y ) + c(-1, 1)

## aspect ratio of plot
my.aspect <- diff( arena_x ) / diff( arena_y )

## quick tracks
ggplot( res, aes( x, y, colour = state, group = id ) ) +
  theme_minimal() +
  geom_path( size = 0.5 ) +
  theme( aspect.ratio = my.aspect ) +
  coord_flip()

## tile grid
tile.grid <- melt( landscape )

## fix label
tile.grid$Var2 <- as.numeric( gsub( 'V', '', tile.grid$Var2 ) )

## -
## work
## -
for( f in 2 : max( res$time ) ) {
  
  ## grab a dataframe you want to use
  current <- res[res$time <= f & res$time > (f - 10), ]
  
  ## calculate transparency based on time
  current$transp <- exp( current$time / max( current$time ) )
  
  ## index for the current time
  current$curr <- rep( 0 )
  current$curr[current$time == max(current$time)] <- 1
  
  ##
  ## can i show when groups initiate? 
  ##
  
  # ## start by grabbing the adjacency matrix
  # A <- adj[[ f ]]
  # 
  # ## make ggnet object
  # net <- network(A, directed = T)
  # 
  # ## edge weights
  # net %e% 'weights' = A[A > 0]
  # 
  # ## vertex state
  # net %v% 'state' <- current[current$curr == 1, ]$state %>% as.character()
  # 
  # ## vertex type
  # net %v% 'type' <- current[current$curr == 1, ]$in.a.patch %>% as.character()
  # 
  # ## fortify for plotting
  # gnet <- ggnetwork(net, arrow.gap = 0.045,
  #                   layout = matrix(c(current[current$curr == 1 & current$id != 'Predator', ]$x,
  #                                     current[current$curr == 1 & current$id != 'Predator', ]$y), ncol = 2),
  #                   scale = F)
  
  ## order factors in color mapping
  myColors <- c( '#1d91c0', '#e41a1c', '#fdae61' )
  names(myColors) <- c('Feed', 'Flee', 'Cool')
  colScale <- scale_colour_manual(values = myColors, guide = F)
  
  ## ggnetwork plot
  gg.net <- ggplot(data = current, aes(x = x, y = y, group = id, 
                                       colour = state, 
                                       alpha = transp ) ) +
    # geom_tile(data = tile.grid, 
    #           aes( Var1, Var2, fill = value ), 
    #           colour = '#000000',
    #           alpha = 0.5,
    #           size = NA,
    #           inherit.aes = F ) +
    # geom_edges(color = '#ffffff',
    #            size = 0.2,
    #            alpha = 0.1,
    #            arrow = arrow( length = unit( 0, 'pt' ), type = 'closed' ) ) + # draw edge layer
    geom_path(size = 0.1, colour = '#d9d9d9' ) +
    geom_point( aes( colour = as.factor( state ), size = state ), shape = 20, alpha = 0.55 ) + # draw node layer
    theme_blank() +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits = arena_x) +
    scale_y_continuous(limits = arena_y) +
    scale_fill_manual( values = c( '#737373', '#000000' ), guide = F ) +
    colScale +
    scale_shape_manual(values = c(21, 23), guide = F) +
    scale_size_manual(values = c( 'Flee' = 0.1, 'Feed' = 0.025, 'Cool' = 0.05 ), guide = F ) +
    scale_alpha_continuous(range = c( 0.1, 1 ), trans = 'sqrt', guide = F ) +
    theme( aspect.ratio = my.aspect,
           panel.background = element_rect(fill = "black", color  =  NA),
           plot.background = element_rect(color = "black", fill = "black") ) +
    coord_flip()
  
  
  ##
  ## viz output
  ##
  
  ## generate a filename for the frame
  fn <- paste('../animation_assets/z_frame', 
              sprintf("%05d", f), 
              sep = '_') %>%
    paste(., 'png', sep = '.')
  
  ## vector
  png(filename = fn,
      width     = 2.95,
      height    = 2.95,
      units     = 'in',
      res       = 480,
      bg = 'black'
  )
  
  ## print network
  print(gg.net)
  
  ## print
  dev.off()
  
  ## drop a line
  cat(f, 'of', n, 'printed\n')
  
}









