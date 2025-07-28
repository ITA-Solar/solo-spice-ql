PRO test_l3_fitting
  use_l2_file = 0
  IF use_l2_file THEN BEGIN
     f = "$SPICE_DATA/level2/2023/10/28/solo_L2_spice-n-ras_20231028T005506_V22_218104189-003.fits"
     obj = spice_object(f)
     ana = obj->xcfit_block(0)
     g = obj.create_l3_file(0)
  END ELSE BEGIN 
     f = "$SPICE_DATA/level3/2023/10/28/solo_L3_spice-n-ras_20231028T005506_V23_218104189-003.fits"
     f = "$SPICE_DATA/level3/2025/05/11/solo_L3_spice-n-ras_20250511T045328_V04_318767540-024.fits"
     obj = spice_object(f)
     ana = obj->xcfit_block(0)
     ana = obj->xcfit_block(1)
     ana = obj->xcfit_block(2)
     ana = obj->xcfit_block(3)
     ana = obj->xcfit_block(4)
     ana = obj->xcfit_block(5)
     ana = obj->xcfit_block(6)
     ana = obj->xcfit_block(7)
     ana = obj->xcfit_block(8)
  END
  stop
END

test_l3_fitting
END
