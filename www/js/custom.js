// Real-time clock script
function updateClock() {
    const clockElement = document.getElementById("dynamic-clock");
    if (clockElement) {
      const now = new Date();
      const timeString = now.toLocaleTimeString([], { 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit' 
      });
      clockElement.textContent = timeString;
    }
  }
  
  // Update the clock every second
  setInterval(updateClock, 1000);

  

  // Show or Hide Instructions

  // Wait until the DOM is fully loaded
  document.addEventListener("DOMContentLoaded", function() {
    
    // Because Shiny namespaces IDs, you must watch for 
    // the final ID that includes the namespace. 
    // However, if your module ID is "data_overview_id", 
    // the final button ID in the DOM might be something like "data_overview_id-toggle_instructions_btn"
    // so you can dynamically add an observer with Shiny.inputBindings 
    // or simply query using something like:
    
    const toggleBtn = document.querySelector("[id$='toggle_instructions_btn']");  // ends with toggle_instructions_btn
    const instructionsDiv = document.getElementById("dataUploadInstructions");
    
    // Initially hide the instructions when the page loads
    if (instructionsDiv) {
      instructionsDiv.style.display = "none";
    }

    if (toggleBtn && instructionsDiv) {
      toggleBtn.addEventListener("click", function() {
        if (instructionsDiv.style.display === "none") {
          instructionsDiv.style.display = "block";
        } else {
          instructionsDiv.style.display = "none";
        }
      });
    }
  });

  
  
  