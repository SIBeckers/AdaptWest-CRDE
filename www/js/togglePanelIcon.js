$(document).ready(function(){
  $(".mycolIcon").click(function(args){
    var el = args.target;
    var target = el.dataset.target;
    var parent = document.getElementById("patourPanel");
    if ($(target).hasClass("collapsing")){
      return;
    }
    
    if ($(el).hasClass('collapsed')) {
        $(el).removeClass('fa-plus ').addClass('fa-minus');
        parent.classList.remove("mystyle");
    } else {
      $(el).removeClass('fa-minus ').addClass('fa-plus');
      parent.classList.add("mystyle");
    }
    
  }); 
  
});
