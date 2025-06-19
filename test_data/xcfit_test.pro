pro xcfit_test

if 0 then begin
file = '/Users/mawiesma/data/spice/user/level3/2024/01/01/solo_L3_spice-n-exp_20240101T180040_V02_234881025-000.fits'
ana=fits2ana(file)
save_analysis,ana[0]
endif else begin
file = '../ancillary/xcftit_test_file.ana'
ana=restore_analysis(file)
endelse
end
