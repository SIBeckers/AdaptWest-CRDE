$(document).ready(function(){
  $(".mycolIcon").click(function(args){
    var el = args.target;
    var target = el.dataset.target;
    if ($(target).hasClass("collapsing")){
      return;
    }
    
    if ($(el).hasClass('collapsed')) {
        $(el).removeClass('fa-plus ').addClass('fa-minus');
        //document.getElementById("climexpPanel").style.width="50px";
    } else {
      $(el).removeClass('fa-minus ').addClass('fa-plus');
    }
    
  }); 
  
});
