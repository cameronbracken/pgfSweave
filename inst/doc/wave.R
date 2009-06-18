syms <-c('alpha','theta','tau','beta','vartheta','pi','upsilon',
		  'gamma','gamma','varpi','phi','delta','kappa','rho','varphi',
		  'epsilon','lambda','varrho','chi','varepsilon','mu','sigma',
		  'psi','zeta','nu','varsigma','omega','eta','xi','Gamma',
		  'Lambda','Sigma','Psi','Delta','Xi','Upsilon','Omega',
		  'Theta','Pi','Phi')	  
r <- runif(length(syms)); g <- runif(length(syms)); b <- runif(length(syms))
t <- seq(-2*pi,2*pi,length.out=length(syms))
R <- exp(.1*t)
x <- R*sin(t); y <- R*cos(t)
plot(0,type='n',xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),axes=F,
	    xlab='',ylab='')
box()
text(x,y,paste('\\color[rgb]{',r,',',g,',',b,'}{$\\',syms,'$}',sep=''))
