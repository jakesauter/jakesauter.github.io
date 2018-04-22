<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="https://bootswatch.com/4/darkly/bootstrap.min.css">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
    <link href='http://fonts.googleapis.com/css?family=Great+Vibes' rel='stylesheet' type='text/css'>
    <link rel="stylesheet" type="text/css" href="index.css" />
    <script src="jquery.js"></script>
    <script src="index.js"></script>
    <title>Jake Sauter</title>
  </head>
  <body>
    <div id="mySidenav" class="sidenav">
      <a href="javascript:void(0)" class="closebtn" onclick="closeNav()">&times;</a>
      <a href="course-sites/csc416.php">Artificial Intelligence 1</a>
      <a href="course-sites/csc466.php">Artificial Intelligence 2</a>
      <a href="course-sites/cog376.php">Natural Language Processing</a>
      <a href="course-sites/csc322.php">Systems Programming</a>
    </div>
    <!-- Use any element to open the sidenav -->


    <!-- Add all page content inside this div if you want the side nav to push page content to the right (not used if you only want the sidenav to sit on top of the page -->
    <div id="main">
      <div class="container-fluid">
        <div class="center">
          <h1><a href="index.php" style="text-decoration:none;color:white;">Jake Sauter</a></h1>
        </div>
        <span onclick="openNav()"> <button class="btn btn-primary" type="button"
          name="button">Courses</button></span>
        <!-- Example row of columns -->
        <div class="row">
          <div class="col-xs-3">
            <h2>Research</h2>
            <img src="http://uconn-today.universityofconn.netdna-cdn.com/wp-content/uploads/2016/09/NSF-Featured-News-Image.jpg" alt="Image Not Found">
            <p> Research experience coming through courses at SUNY Oswego
                and a great REU at Depaul University and University of Chicago
                in the field of Medical Informatics</p>
            <p><a class="btn btn-default" href="research.php" 
              style="margin-top:-2px"role="button">Research Experience &raquo;</a></p>
          </div>
          <div class="col-xs-3">
            <h2>Teaching</h2>
            <img src="http://www.summercamps.com/wp-content/uploads/2017/02/1030_geodir_logo_245165.jpg" alt="Image Not Found">
            <p> Teaching in a wide range of envrionments from after school in high school to a
                professional technology summer camp tutoring/crash course program.</p>
            <p><a class="btn btn-default" href="/~jsauter/teaching.php" role="button">Teaching Experience &raquo;</a></p>
         </div>
          <div class="col-xs-3">
            <h2>Scholarship</h2>
            <img src="http://www.commonapp.org/files/styles/400-y/public/school/logo/SUNY_Oswego-Logo_500x500.jpg?itok=4yorYjAE" alt="Image Not Found">
            <p> Achievment at the highest level academically, achieving many awards for scholarship in the fields of Computer Science, Mathematics, Physics and Cognitive Science.</p>
            <p><a class="btn btn-default" href="transcripts.php" role="button">Transcipts &raquo;</a></p>
          </div>
        </div>
        <div id="social">
          <a class="btn btn-secondary btn-lg social" href="https://www.linkedin.com/in/jake-sauter-532399109/">LinkedIn</a>
          <a class="btn btn-secondary btn-lg social" href="https://github.com/ahscodingclub">Github</a>
          <a class="btn btn-secondary btn-lg social" href="https://www.researchgate.net/profile/Jake_Sauter">ResearchGate</a>
        </div>
        <div class="card" style="width: 20em;" id="card">
          <div class="card-block">
						<h4 class="card-title">Contact Me</h4>
          	<ul class="list-group list-group-flush">
            <li class="list-group-item">Cell: 845-891-9910</li>
            <li class="list-group-item">Email: <a href="mailto:jsauter@oswego.edu">jsauter@oswego.edu</a></li>
          </ul>
        </div>
      </div>
  </div>
  </body>
</html>
